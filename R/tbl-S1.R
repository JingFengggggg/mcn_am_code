# tbl-S1 ------------------------------------------------------------------


library(MCnebula2)
library(exMCnebula2)

load("mcn.rdata")
feas <- features_annotation(mcn)
feas <- merge(feas, top_table(statistic_set(mcn))[[1]], by = ".features_id", all.x = T)
feas_a <- mcn@mcn_dataset@reference$features_annotation
com_format <- tibble::tibble(
  Synonym = feas_a$synonym, .features_id = feas_a$.features_id,  
  `Mass error (ppm)` = feas_a$error.mass, Formula = feas_a$mol.formula,
  Adduct = feas_a$adduct, logFC = feas$logFC, 
  `P-value` = feas$P.Value,
  `Sirius score` = feas_a$sirius.score,
  `Tanimoto similarity` = feas_a$tani.score, `InChIKey planar` = feas_a$inchikey2d
)
origin <- data.table::fread("pos_msms.csv")
origin <- tibble::as_tibble(origin)
origin2 <- origin[,c(1, 3)]
colnames(origin2) <- c(".features_id", "RT(min)")
com_format <- merge(com_format, origin2, by = ".features_id", all.x = TRUE)

class_donut <- data.frame(
  .features_id = mcn@mcn_dataset@backtrack$stardust_classes$.features_id,
  classes = mcn@mcn_dataset@backtrack$stardust_classes$class.name
) |> 
  dplyr::group_by(.features_id) |> 
  dplyr::summarise(classes = paste(classes, collapse = ",")) |> 
  dplyr::ungroup()
com_format <- merge(com_format, class_donut, by = ".features_id", all.x = TRUE)

com_format <- 
  dplyr::arrange(com_format, as.numeric(.features_id)) |> 
  dplyr::filter(Adduct == "[M + H]+")
data.table::fwrite(com_format, paste0(getwd(), "/temp_data/compounds_format_pos.csv"))

com_format <- 
  dplyr::arrange(com_format, as.numeric(.features_id)) |> 
  dplyr::filter(Adduct == "[M + H]+")
tops <- select_features(
  mcn, order_by_coef = 1, coef=1,
  togather = T
)
com_format <- dplyr::filter(com_format, .features_id %in% tops[1:200])
data.table::fwrite(com_format, paste0(getwd(), "/temp_data/compounds_format_pos200.csv"))
