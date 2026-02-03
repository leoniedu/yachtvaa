#' Title Case Portuguese Strings with Special Capitalization Rules
#'
#' This function converts strings to title case while applying special rules for
#' Portuguese grammar, particularly for articles, prepositions, and other small words
#' that should remain lowercase in titles.
#'
#' @param string A character vector to be capitalized according to Portuguese title case rules
#' @param ... Additional arguments passed to snakecase::to_title_case()
#' @return A character vector with properly capitalized strings according to Portuguese title case conventions
#' @examples
#' capitalizar("o senhor dos anéis: a sociedade do anel")
#' # Returns: "O Senhor dos Anéis: A Sociedade do Anel"
#' @export
capitalizar <- function(string, ...) {
  stringr::str_replace_all(
    snakecase::to_title_case(string, ...),
    c(
      `(.)\\bA(s)?\\b` = "\\1a\\2",
      `(.)\\bO(s)?\\b` = "\\1o\\2",
      `(.)\\bU((m(a(s)?)?)|ns)\\b` = "\\1u\\2",
      `(.)\\bL(he(s)?)\\b` = "\\1l\\2",
      `(.)\\bM((e(u(s)?)?)|(i(m|(nha(s)?))))\\b` = "\\1m\\2",
      `(.)\\bN(os(s[ao](s)?)?)\\b` = "\\1n\\2",
      `(.)\\bS((e(u(s)?)?)|(ua(s)?))\\b` = "\\1s\\2",
      `(.)\\bT((e(u(s)?)?)|i|(ua(s)?))\\b` = "\\1t\\2",
      `(.)\\bV(os(s[ao](s)?)?)\\b` = "\\1v\\2",
      `(.)\\bA((o)(s)?|nte|té|pós)\\b` = "\\1a\\2",
      `(.)\\bÀ(s)?\\b` = "\\1à\\2",
      `(.)\\bC(om|ontra)\\b` = "\\1c\\2",
      `(.)\\bD(((a|o)(s)?)|(e(sde)?))\\b` = "\\1d\\2",
      `(.)\\bE(m|ntre)\\b` = "\\1e\\2",
      `(.)\\bN((a|o)(s)?)\\b` = "\\1n\\2",
      `(.)\\bP(ara|(e((l(a|o)(s)?)|rante))|or)\\b` = "\\1p\\2",
      `(.)\\bS(em|(ob(re)?))\\b` = "\\1s\\2",
      `(.)\\bT(rás)\\b` = "\\1t\\2",
      `(.)\\bC(on(forme|quanto|tudo))\\b` = "\\1c\\2",
      `(.)\\bD(urante)\\b` = "\\1D\\2",
      `(.)\\bE((mbora|n(quanto|t(ão|retanto))|xceto)?)\\b` = "\\1e\\2",
      `(.)\\bL(ogo)\\b` = "\\1l\\2",
      `(.)\\bM(as)\\b` = "\\1m\\2",
      `(.)\\bN(em)\\b` = "\\1n\\2",
      `(.)\\bO(u|ra)\\b` = "\\1o\\2",
      `(.)\\bP(o(is|r(ém|qu(e|anto)|tanto)))\\b` = "\\1p\\2",
      `(.)\\bQ(u(an[dt]o|e))\\b` = "\\1q\\2",
      `(.)\\bS(e(não)?)\\b` = "\\1s\\2",
      `(.)\\bT(odavia)\\b` = "\\1t\\2"
    )
  )
}
