
## Init lists
models_base<-list()

models_understory<-list()

## ---------------------------------------------------------------------------- ####
## ------------------------------ Model reading ------------------------------- ####
## Basic models ####
models_base$model.eu.pcab.big<-readRDS("calculated_models/base_models/eu_pcab_big.rds")
models_base$model.bb.pcab.big<-readRDS("calculated_models/base_models/bb_pcab_big.rds")
models_base$model.bb.fasy.big<-readRDS("calculated_models/base_models/bb_fasy_big.rds")
models_base$model.zf.pcab.big<-readRDS("calculated_models/base_models/zf_pcab_big.rds")
models_base$model.zf.fasy.big<-readRDS("calculated_models/base_models/zf_fasy_big.rds")
models_base$model.rn.acca.big<-readRDS("calculated_models/base_models/rn_acca_big.rds")
models_base$model.rn.cabe.big<-readRDS("calculated_models/base_models/rn_cabe_big.rds")
models_base$model.rn.ulsp.big<-readRDS("calculated_models/base_models/rn_ulsp_big.rds")

models_base$model.eu.pcab.small<-readRDS("calculated_models/base_models/eu_pcab_small.rds")
models_base$model.bb.pcab.small<-readRDS("calculated_models/base_models/bb_pcab_small.rds")
models_base$model.bb.fasy.small<-readRDS("calculated_models/base_models/bb_fasy_small.rds")
models_base$model.zf.pcab.small<-readRDS("calculated_models/base_models/zf_pcab_small.rds")
models_base$model.zf.fasy.small<-readRDS("calculated_models/base_models/zf_fasy_small.rds")
models_base$model.rn.acca.small<-readRDS("calculated_models/base_models/rn_acca_small.rds")
models_base$model.rn.cabe.small<-readRDS("calculated_models/base_models/rn_cabe_small.rds")
models_base$model.rn.ulsp.small<-readRDS("calculated_models/base_models/rn_ulsp_small.rds")

## Understory models ####
models_understory<-models_base[c("model.eu.pcab.big",
                                 "model.bb.pcab.big",
                                 "model.bb.fasy.big",
                                 "model.zf.pcab.big",
                                 "model.zf.fasy.big",
                                 "model.rn.acca.big",
                                 "model.rn.cabe.big",
                                 "model.rn.ulsp.big")]

models_understory$model.eu.pcab.small<-readRDS("calculated_models/understory_models/eu_pcab_small_understory.rds")
models_understory$model.bb.pcab.small<-readRDS("calculated_models/understory_models/bb_pcab_small_understory.rds")
models_understory$model.bb.fasy.small<-readRDS("calculated_models/understory_models/bb_fasy_small_understory.rds")
models_understory$model.zf.pcab.small<-readRDS("calculated_models/understory_models/zf_pcab_small_understory.rds")
models_understory$model.zf.fasy.small<-readRDS("calculated_models/understory_models/zf_fasy_small_understory.rds")
models_understory$model.rn.acca.small<-readRDS("calculated_models/understory_models/rn_acca_small_understory.rds")
models_understory$model.rn.cabe.small<-readRDS("calculated_models/understory_models/rn_cabe_small_understory.rds")
models_understory$model.rn.ulsp.small<-readRDS("calculated_models/understory_models/rn_ulsp_small_understory.rds")

