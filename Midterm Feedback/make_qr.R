## Make a QR code for the midterm feedback survey.
##   Rscript make_qr.R "https://duke.qualtrics.com/jfe/form/SV_xxxxxxxx"
## Writes qr_code.png (for slides / handouts) and qr_code.pdf (vector, for LaTeX).
url <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(url)) stop("give the survey's anonymous link as the first argument")
if (!requireNamespace("qrcode", quietly = TRUE)) install.packages("qrcode")
code <- qrcode::qr_code(url, ecl = "M")         ## medium error correction: survives a projector
png("qr_code.png", width = 1200, height = 1200, res = 300); par(mar = c(0, 0, 0, 0))
plot(code); dev.off()
pdf("qr_code.pdf", width = 4, height = 4); par(mar = c(0, 0, 0, 0))
plot(code); dev.off()
cat("wrote qr_code.png and qr_code.pdf for", url, "\n")
