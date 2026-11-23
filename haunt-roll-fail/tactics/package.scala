package object tactics extends hrf.base.Gaming with hrf.bot.BotGaming with hrf.ui.GreyMapUI {
    type F = Faction
    type G = Game

    val gaming = this

    val styles = elem.styles
}
