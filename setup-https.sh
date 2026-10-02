#!/usr/bin/env bash
# One-time https setup for the live server (see CLAUDE.md, "Setting up https").
# Run on the server as ubuntu, while the server is running this branch:
#   ./setup-https.sh [hostname] [email]
# Default hostname: <public-ip-with-dashes>.sslip.io
set -euo pipefail

if [ "$(uname)" != Linux ] || ! command -v netfilter-persistent > /dev/null; then
    echo "Run this on the server, not here: ssh -i ~/.ssh/oracle.key ubuntu@157.151.177.11" >&2
    exit 1
fi

HRF_DIR="$(cd "$(dirname "$0")" && pwd)"
GG_DIR="$HRF_DIR/good-game"
IP="$(curl -s https://api.ipify.org)"
HOST="${1:-${IP//./-}.sslip.io}"
EMAIL="${2:-}"
USER_NAME="$(id -un)"

echo "== Hostname: $HOST (public IP $IP)"

if [ "$(getent hosts "$HOST" | awk '{print $1}')" != "$IP" ]; then
    echo "$HOST does not resolve to $IP" >&2
    exit 1
fi

echo "== Allowing non-root programs to use ports 80 and 443"
echo "net.ipv4.ip_unprivileged_port_start=80" | sudo tee /etc/sysctl.d/50-hrf-ports.conf > /dev/null
sudo sysctl -q -p /etc/sysctl.d/50-hrf-ports.conf

echo "== Opening ports 80 and 443 in iptables"
for p in 80 443; do
    if ! sudo iptables -C INPUT -p tcp -m state --state NEW -m tcp --dport $p -j ACCEPT 2>/dev/null; then
        line="$(sudo iptables -L INPUT --line-numbers | awk '/REJECT/ {print $1; exit}')"
        sudo iptables -I INPUT "${line:-1}" -p tcp -m state --state NEW -m tcp --dport $p -j ACCEPT
    fi
done
sudo netfilter-persistent save > /dev/null

echo "== Installing certbot"
if ! command -v certbot > /dev/null; then
    sudo apt-get update -q
    sudo apt-get install -y -q certbot
fi

echo "== Checking that the server answers Let's Encrypt challenges on port 80"
mkdir -p "$GG_DIR/acme/.well-known/acme-challenge"
echo ok > "$GG_DIR/acme/.well-known/acme-challenge/hrf-check"
if [ "$(curl -s http://localhost/.well-known/acme-challenge/hrf-check)" != "ok" ]; then
    echo "Nothing is serving $GG_DIR/acme on port 80." >&2
    echo "Restart the server so it starts its port 80 server (it does when good-game/acme exists)," >&2
    echo "check that ports 80 and 443 are open in the Oracle security list, then run this again." >&2
    exit 1
fi
rm "$GG_DIR/acme/.well-known/acme-challenge/hrf-check"

echo "== Installing the renewal hook"
HOOK=/etc/letsencrypt/renewal-hooks/deploy/hrf-pkcs12.sh
sudo mkdir -p "$(dirname "$HOOK")"
sudo tee "$HOOK" > /dev/null <<EOF
#!/bin/sh
# Converts the Let's Encrypt certificate to the certificate.pkcs12 file the HRF server reads.
# The server notices the new file within a minute; no restart needed.
set -e
LINEAGE="\${RENEWED_LINEAGE:-/etc/letsencrypt/live/$HOST}"
# Only the certificate for the hostname the server runs on
[ "\$LINEAGE" = "/etc/letsencrypt/live/$HOST" ] || exit 0
OUT="$GG_DIR/certificate.pkcs12"
openssl pkcs12 -export -in "\$LINEAGE/fullchain.pem" -inkey "\$LINEAGE/privkey.pem" -out "\$OUT.tmp" -passout pass:
chown $USER_NAME: "\$OUT.tmp"
chmod 600 "\$OUT.tmp"
mv "\$OUT.tmp" "\$OUT"
EOF
sudo chmod 755 "$HOOK"

echo "== Getting the certificate"
if [ -n "$EMAIL" ]; then
    ACCOUNT=(-m "$EMAIL")
else
    ACCOUNT=(--register-unsafely-without-email)
fi
sudo certbot certonly --webroot -w "$GG_DIR/acme" -d "$HOST" \
    --non-interactive --agree-tos "${ACCOUNT[@]}" --keep-until-expiring
sudo RENEWED_LINEAGE="/etc/letsencrypt/live/$HOST" "$HOOK"

echo "== Testing renewal"
sudo certbot renew --dry-run -q && echo "Renewal works."

cat <<EOF

Done. certificate.pkcs12 is in $GG_DIR.
Certbot renews it automatically (systemd certbot.timer).

Now restart the server on port 443 (Ctrl-C it in 'tmux attach -t hrf', then):
  cd $GG_DIR
  sbt "run run ../good-game-database ../haunt-roll-fail https://$HOST https://$HOST/hrf/ 443"

The site is then at https://$HOST/play
EOF
