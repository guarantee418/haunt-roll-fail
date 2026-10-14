// Loads the live site in Chrome, opens a game's menu, starts a Quick Game
// and reports the public IP used, load times, console errors, failed
// requests and a screenshot of each stage (see README.md).

const fs = require('fs')
const path = require('path')
const puppeteer = require('puppeteer')

const args = { url: 'https://games.clean5110.com', meta: 'nort', mode: 'quick', out: 'site-test-output', timeout: 180, viewport: '1600x900' }

for (let i = 2; i < process.argv.length; i++) {
    const a = process.argv[i]
    if (a === '--headful') args.headful = true
    else if (a === '--help' || a === '-h') args.help = true
    else if (a.startsWith('--') && i + 1 < process.argv.length) args[a.slice(2)] = process.argv[++i]
    else { console.error('Unknown argument: ' + a); process.exit(2) }
}

if (args.help) {
    console.log(`Usage: node site-test.js [options]

  --url <url>          site to test (default ${args.url})
  --meta <name>        game on /play/<name> (default ${args.meta})
  --mode quick|menu    start a Quick Game, or only load the menu (default ${args.mode})
  --viewport WxH       window size, e.g. 390x844 for a phone (default ${args.viewport})
  --timeout <seconds>  how long to wait for each stage (default ${args.timeout})
  --out <dir>          where screenshots and report.json go (default ${args.out})
  --headful            show the browser window
  --chrome <path>      use this Chrome instead of Puppeteer's own
  --proxy <url>        send the browser's traffic through this proxy`)
    process.exit(0)
}

const sleep = ms => new Promise(r => setTimeout(r, ms))
const [width, height] = args.viewport.split('x').map(Number)
const timeout = Number(args.timeout) * 1000
const started = Date.now()
const seconds = () => ((Date.now() - started) / 1000).toFixed(1)

const report = { url: args.url, meta: args.meta, mode: args.mode, viewport: args.viewport, startedAt: new Date().toISOString(), stages: [], console: [], pageErrors: [], failedRequests: [] }

function stage(name, ok, extra) {
    const s = Object.assign({ name, ok, at: Number(seconds()) }, extra || {})
    report.stages.push(s)
    console.log(`[${s.at}s] ${ok ? 'OK  ' : 'FAIL'} ${name}${extra && extra.note ? ' (' + extra.note + ')' : ''}`)
}

// Clicks the last visible clickable element (the site marks them with
// data-onclick) whose text starts with the given text, with the mouse.
async function clickText(page, text) {
    const at = await page.evaluate(t => {
        const found = [...document.querySelectorAll('[data-onclick]')].filter(e => e.getClientRects().length && (e.innerText || '').trim().startsWith(t))
        if (!found.length) return null
        const r = found[found.length - 1].getBoundingClientRect()
        return [r.x + r.width / 2, r.y + r.height / 2]
    }, text)
    if (at) await page.mouse.click(at[0], at[1])
    return !!at
}

async function main() {
    fs.mkdirSync(args.out, { recursive: true })

    const launch = { headless: !args.headful, defaultViewport: { width, height }, args: [`--window-size=${width},${height}`] }
    if (args.chrome) launch.executablePath = args.chrome
    if (args.proxy) launch.args.push('--proxy-server=' + args.proxy)
    // Chrome refuses to run as root (containers) with its sandbox on
    if (process.getuid && process.getuid() === 0) launch.args.push('--no-sandbox')

    const browser = await puppeteer.launch(launch)
    const page = await browser.newPage()
    report.browser = await browser.version()

    page.on('console', m => {
        if (m.type() === 'error' || m.type() === 'warn' || m.type() === 'warning')
            report.console.push({ at: Number(seconds()), type: m.type(), text: m.text() })
    })
    page.on('pageerror', e => report.pageErrors.push({ at: Number(seconds()), text: e.message }))
    const inflight = new Set()
    let lastHttp = Date.now()
    const isHttp = r => r.url().startsWith('http')
    page.on('request', r => { if (isHttp(r)) { inflight.add(r); lastHttp = Date.now() } })
    page.on('requestfinished', r => { if (inflight.delete(r)) lastHttp = Date.now() })
    page.on('requestfailed', r => { if (inflight.delete(r)) lastHttp = Date.now() })
    page.on('requestfailed', r => report.failedRequests.push({ at: Number(seconds()), url: r.url(), error: r.failure() && r.failure().errorText }))
    page.on('response', r => {
        if (r.status() >= 400) report.failedRequests.push({ at: Number(seconds()), url: r.url(), status: r.status() })
    })

    let shots = 0
    const shot = async name => {
        const file = path.join(args.out, `${String(++shots).padStart(2, '0')}-${name}.png`)
        await page.screenshot({ path: file })
        return file
    }

    try {
        const menuUrl = `${args.url}/play/${args.meta}`
        const response = await page.goto(menuUrl, { waitUntil: 'domcontentloaded', timeout })
        stage('page loaded', response.ok(), { note: `HTTP ${response.status()}` })

        await page.waitForFunction(() => [...document.querySelectorAll('[data-onclick]')].some(e => e.getClientRects().length), { timeout })
        stage('menu shown', true, { screenshot: await shot('menu') })

        // Asked from the page, so it is the address the site sees the browser at
        try {
            report.publicIp = await page.evaluate(() => fetch('https://api.ipify.org?format=json').then(r => r.json()).then(j => j.ip))
            stage('public IP', true, { note: report.publicIp })
        } catch (e) {
            stage('public IP', false, { note: e.message })
        }

        if (args.mode === 'quick') {
            if (!await clickText(page, 'Quick Game')) throw new Error('no "Quick Game" button on the menu')
            // A fresh browser has no saved games, so this goes straight to a new game;
            // it is ready once the player panels are drawn (for Northgard, their ledger tables).
            await page.waitForFunction(nort => document.querySelector(nort ? '[class*="nort-ledger"]' : 'canvas'), { timeout }, args.meta.startsWith('nort'))
            stage('game shown', true)
            // then until the map is drawn (a canvas showing more than a few colors),
            await page.waitForFunction(() => [...document.querySelectorAll('canvas')].some(c => {
                if (c.width < 100 || c.height < 100) return false
                try {
                    const small = document.createElement('canvas')
                    small.width = small.height = 200
                    const g = small.getContext('2d')
                    g.drawImage(c, 0, 0, 200, 200)
                    const d = g.getImageData(0, 0, 200, 200).data
                    const colors = new Set()
                    for (let i = 0; i < d.length; i += 4) colors.add(d[i] >> 4 << 8 | d[i + 1] >> 4 << 4 | d[i + 2] >> 4)
                    return colors.size > 20
                } catch (e) { return false }
            }), { timeout, polling: 1000 })
            stage('map drawn', true)
            // then until its images are in: no http(s) request for 3 seconds
            // (the map keeps reading cached images as blob: URLs as it redraws)
            const until = Date.now() + timeout
            while (Date.now() - lastHttp < 3000 || inflight.size) {
                if (Date.now() > until) throw new Error('images still loading after ' + args.timeout + 's')
                await sleep(250)
            }
            await sleep(2000)
            stage('quick game ready', true, { screenshot: await shot('game') })

            const choices = await page.evaluate(() => [...document.querySelectorAll('[data-onclick]')].filter(e => e.getClientRects().length).map(e => (e.innerText || '').trim()).filter(t => t.length > 1 && t.length < 60))
            report.choicesShown = [...new Set(choices)].slice(0, 30)
        }
    } catch (e) {
        stage('error', false, { note: e.message, screenshot: await shot('error').catch(() => null) })
    }

    report.seconds = Number(seconds())
    report.ok = report.stages.every(s => s.ok) && report.pageErrors.length === 0
    fs.writeFileSync(path.join(args.out, 'report.json'), JSON.stringify(report, null, 2))

    console.log(`\nBrowser: ${report.browser}`)
    console.log(`Console errors/warnings: ${report.console.length}, page errors: ${report.pageErrors.length}, failed requests: ${report.failedRequests.length}`)
    for (const e of report.pageErrors) console.log('  page error: ' + e.text.slice(0, 200))
    for (const e of report.console.filter(c => c.type === 'error').slice(0, 10)) console.log('  console error: ' + e.text.slice(0, 200))
    for (const r of report.failedRequests.slice(0, 10)) console.log(`  failed: ${r.status || r.error} ${r.url.slice(0, 150)}`)
    console.log(`\n${report.ok ? 'PASSED' : 'FAILED'} in ${report.seconds}s. Screenshots and report.json are in ${path.resolve(args.out)}`)

    await browser.close()
    process.exit(report.ok ? 0 : 1)
}

main().catch(e => { console.error(e); process.exit(1) })
