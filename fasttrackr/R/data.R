#' Example Fast Track aggregated data
#'
#' An example of aggregated data as provided by Fast Track (in an 'aggregated_data.csv' file). The data is productions of 12 vowels by 7 adult males from the Hillenbrand et al. (1995) data. The formant columns are so that column FXY represents the frequency for the Xth formant, for the Yth time slice.
#'
#' @section Columns:
#' \describe{
#' \item{file}{the recording file name.}
#' \item{f0}{average f0 in Hertz.}
#' \item{duration}{vowel duration in milliseconds.}
#' \item{label}{The label used for plotting the vowel.}
#' \item{group}{A group number, used for plotting}
#' \item{color}{The color Praat will use for plotting.}
#' \item{number}{The file number.}
#' \item{f11}{The average frequency for the first formant, for the first time slice.}
#' \item{f21}{The average frequency for the second formant, for the first time slice.}
#' \item{...}{And so on.}
#' }
#' @source \url{https://github.com/santiagobarreda/FastTrackR}
#' @references Hillenbrand, J.M., Getty, L.A., Clark, M.J., and Wheeler, K. (1995). "Acoustic characteristics of American English vowels," Journal of the Acoustical Society of America, 97, 3099-3111.

"aggregated_data"

#' Example of Fat Track regressions coefficients
#'
#' An example of the analysis regression coefficients provided by Fast Track (in an 'coefficients.csv' file). The data is productions of 12 vowels by 7 adult males from the Hillenbrand et al. (1995) data. The coefficients columns are so that column cXY represents the coefficient for the Xth formant, for the Yth time slice.
#'
#' Data representing productions of 12 vowels by x adult males from the
#' Hillenbrand et al. (1995) data.
#'
#' @section Columns:
#' \describe{
#' \item{file}{the recording file name.}
#' \item{c11}{The 0th order coefficient (intercept) for the for the first formant.}
#' \item{c21}{A series of columns, each representing the Yth order coefficients for formant X.}
#' \item{...}{And so on.}
#' }
#'
#' @source \url{https://github.com/santiagobarreda/FastTrackR}
#' @references Hillenbrand, J.M., Getty, L.A., Clark, M.J., and Wheeler, K. (1995). "Acoustic characteristics of American English vowels," Journal of the Acoustical Society of America, 97, 3099-3111.

"coefficients"

