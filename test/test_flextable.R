library(flextable)
library(officer)

test = cbind(var = colnames(cbind(bvaq, panas)),
             corstars(cbind(bvaq, panas))
             ) %>% flextable() %>% autofit()

docx_file <- tempfile(fileext = ".docx")
pptx_file <- tempfile(fileext = ".pptx")
# docx_file <- "example.docx"
pptx_file <- "example.pptx"
save_as_docx("my table" = test, path = docx_file)
save_as_pptx("my table" = test, path = pptx_file)
