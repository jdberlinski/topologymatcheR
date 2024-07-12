#' Read data quickly, from CSV files
#'
#' The images should now exist in a 512 by 1023 (or other) matrix that is stored in a CSV file, with one file per image.
#' This function will loop through files in a specified directory and either:
#'   1) stores the 6 image pairs for each surface in a list which is returned
#'   2) creates the list in 1), and computes a data frame of correlation which is returned

#' @param img_no numeric; number of images per surface
#' @param sample_names character vector; each element is a string corresponding to the fractured surface being
#' investigated
#' @param operator character; who imaged the sample, used to construct the end of the file to be read, with `file_id`
#' @param fdir character; director which holds the csv files
#' @param file_id the end of the filename, typically "_FFT", which will be used to form the filename '.*operator_FFT.csv'
#' @param surface_names character vector; what are the names of the two pairs? they are usually base and tip
#' @param dim; numeric vector; size of the FFT matrix
#' @param prepend_zero logical; for images 1-9, does the filename format as 01-09?
#' @param verbose logical; should progress be printed?
#' @param stopifmissing logical; should the program be halted if a file is missing?
#' @param skip numeric; how many lines at the start correspond to metadata? this is usually 4 for the magnification and
#' resolution. The resolution should be the last line (ie, line number `skip`)
#' @export
#' @author josh berlinski
#' @importFrom data.table fread
fread_fft <- function(
    img_no,
    sample_names,
    operator,
    fdir,
    file_id = "_FFT",
    surface_names = c("Base", "Tip"),
    dim = c(512, 1023),
    prepend_zero = TRUE,
    verbose = TRUE,
    stopifmissing = TRUE,
    skip = 4
) {

    if (prepend_zero) {
        img_names <- paste0("0", 1:img_no)
        if (img_no > 9)
            img_names[10:img_no] <- as.character(10:img_no)
    }
    else {
        img_names <- as.character(1:img_no)
    }

    out_dim <- c("surface"=2, "FFT_row"=dim[1], "FFT_col"=dim[2], "images"=img_no)
    out <- lapply(sample_names, function(x) array(-1, out_dim))

    res_tip <- NA
    res_base <- NA
    res <- NA

    fbase <- paste0(operator, file_id, ".csv")

    for (i in seq_along(sample_names)) {
        sn <- sample_names[i]
        if (verbose) {
            cat("-*^*- started reading data for ", sn, "-*^*-\n")
            cat("\t Images:")
        }
        for (j in seq_along(img_names)) {
            img <- img_names[j]
            if (verbose)
                cat(" ", img)


            bfile <- paste0(fdir, paste(sn, surface_names[1], img, fbase, sep="-"))
            tfile <- paste0(fdir, paste(sn, surface_names[2], img, fbase, sep="-"))

            if (!file.exists(bfile) & stopifmissing)
                stop("\nFile ", bfile, " not found.")
            if (!file.exists(tfile) & stopifmissing)
                stop("\nFile ", tfile, " not found.")

            res_base <- as.numeric(fread(bfile, nrows=1, skip=skip - 1, header=FALSE, data.table=FALSE))
            res_tip <- as.numeric(fread(tfile, nrows=1, skip=skip - 1, header=FALSE, data.table=FALSE))

            if (res_tip != res_base)
                stop("\nResolution of base for sample ", sn, " image ", img, " is ", res_base, " while tip resolution is ", res_tip, ".")
            else if (i != 1 && j != 1 && res_tip != res)
                stop("\nResolution of sample ", sn, " image ", img, " is ", res_base, " while previous resolutions are ", res, ".")
            else
                res <- res_tip

            out[[i]][1, , , j] <- as.matrix(fread(bfile, skip=skip, nrows=dim[1], header=FALSE, data.table=FALSE))
            out[[i]][2, , , j] <- as.matrix(fread(tfile, skip=skip, nrows=dim[1], header=FALSE, data.table=FALSE))

            if (!all(dim(out[[sn]][1, , , i]) == dim(out[[sn]][2, , , i])))
                stop("\nDimensions of base and tip for sample ", sn, " image ", img, " do not match.")
        }
        if (verbose)
            cat("\n")
    }
    names(out) <- sample_names
    return(list(resolution = res, surfaces = out))
}
