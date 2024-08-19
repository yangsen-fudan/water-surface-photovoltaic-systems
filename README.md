# water-surface-photovoltaic-systems
Effects of water surface photovoltaic systems on the water physical and chemical variables and biodiversity
## 1 Project description：----
# The ecological investigation of aquatic photovoltaic region in the Yangtze River Basin of China was carried out to explore the impact of aquatic photovoltaic on wetland ecosystem.
# setwd_R()
setwd_R = function(){setwd(dirname(rstudioapi::getSourceEditorContext()$path))}
setwd_R()
## 2 Data source： ----
# The physical and chemical parameters, plankton and birds of 26 aquatic photovoltaic systems in the Yangtze River Basin were investigated in the winter and summer of 2022.

## 3 Engineering code:-----
## 3.1 Engineering environment：-----
# set common parameters
options(scipen = 1000)
options(pillar.sigfig = 3)
options(encoding = 'UTF-8')

library(pacman)

# load stat package:
p_load(vegan,ade4)

# load common packages:
p_load(purrr,tidyverse,dplyr,stringr,tidyr,rio)
     
# load plot packages:
p_load(ggplot2,ggprism,plotly,ggrepel)

## setwd:
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

## 3.2 Read the original engineering data and preprocess：-----
raw_land_survey_data_union <- import_list("./final_pv_survey_raw_data/raw_land_survey_data_union.xlsx",
                                          col_names = FALSE)
# Name the reset function
convert_and_remove_first_row <- function(data) {
  colnames(data) <- unlist(data[1, ])
  data <- data[-1, ]
  return(data)
}

land_survey_data = raw_land_survey_data_union[["1_land_survey_data"]] %>% 
  convert_and_remove_first_row(.)

phytoplankton_winter = raw_land_survey_data_union[["2_phytoplankton_winter"]][,-1] %>% 
  t() %>% data.frame() %>% convert_and_remove_first_row(.)
  
phytoplankton_summer = raw_land_survey_data_union[["2_phytoplankton_summer"]][,-1] %>% 
  t() %>% data.frame() %>% convert_and_remove_first_row(.) 

rotifers_Winter = raw_land_survey_data_union[["3_rotifers_Winter"]][,-1] %>% 
  t() %>% data.frame() %>% convert_and_remove_first_row(.) 
rotifers_summmer =raw_land_survey_data_union[["3_rotifers_summmer"]][,-1] %>% 
  t() %>% data.frame() %>% convert_and_remove_first_row(.) 

zooplankton_winer = raw_land_survey_data_union[["4_zooplankton_winer"]][,-1] %>% 
  t() %>% data.frame() %>% convert_and_remove_first_row(.) 
zooplankton_summer = raw_land_survey_data_union[["4_zooplankton_summer"]][,-1] %>% 
  t() %>% data.frame() %>% convert_and_remove_first_row(.) 


raw_brid_data_union <- import_list("./final_pv_survey_raw_data/raw_brid_data_union.xlsx",
                                   col_names = FALSE)
brid_union_data = raw_brid_data_union[["1_brid_union_data"]] %>% 
  convert_and_remove_first_row(.) 

brid_number_winter = raw_brid_data_union[["2_brid_number_winter"]] %>% 
  t() %>% data.frame() %>% convert_and_remove_first_row(.) 
brid_number_summer = raw_brid_data_union[["2_brid_number_summer"]] %>% 
  t() %>% data.frame() %>% convert_and_remove_first_row(.) 

brid_density_winter = raw_brid_data_union[["3_brid_density_winter"]]%>% 
  t() %>% data.frame() %>% convert_and_remove_first_row(.) 
brid_density_summer = raw_brid_data_union[["3_brid_density_summer"]] %>% 
  t() %>% data.frame() %>% convert_and_remove_first_row(.) 

bird_ecotypes_number_winter = raw_brid_data_union[["4_bird_ecotypes_number_winter"]] %>% 
  t() %>% data.frame() %>% convert_and_remove_first_row(.) 
bird_ecotypes_number_summer = raw_brid_data_union[["4_bird_ecotypes_number_summer"]]%>% 
  t() %>% data.frame() %>% convert_and_remove_first_row(.) 

bird_ecotype_density_winter = raw_brid_data_union[["5_bird_ecotype_density_winter"]]%>% 
  t() %>% data.frame()  %>% convert_and_remove_first_row(.) 
bird_ecotype_density_summer = raw_brid_data_union[["5_bird_ecotype_density_summer"]] %>% 
  t() %>% data.frame()  %>% convert_and_remove_first_row(.) 

## 3.3 stat-Descriptive comparison of biological and abiotic indicators in different seasons and photovoltaic conditions：-----
## 3.1.2 pari-plot -----
plot_hist_land = function(label,data){

  clean_plot_temp1 <- data %>% 
    dplyr::select("season","site_type",label) %>%
    filter(season == "Winter") %>%  rename(pc_poistion = site_type,label = label) %>%
    mutate(paired = rep(1:(n()/3),each=3),pc_poistion=factor(pc_poistion)) %>% 
    mutate(pc_poistion = factor(pc_poistion,levels = c("PA","NPA","CA")))
  
  clean_plot_temp2 <- data %>%
    dplyr::select("season","site_type",label) %>%
    filter(season == "Summer") %>% rename(pc_poistion = site_type,label = label) %>%
    mutate(paired = rep(1:(n()/3),each=3),pc_poistion=factor(pc_poistion)) %>% 
    mutate(pc_poistion = factor(pc_poistion,levels = c("PA","NPA","CA")))
  
  clean_plot_temp = rbind(clean_plot_temp1,clean_plot_temp2) %>% data.frame() %>% 
    mutate(label = as.numeric(label))
  
  clean_plot_temp %>%
    ggplot(aes(pc_poistion,label,color = pc_poistion)) +
    stat_boxplot(geom="errorbar",position=position_dodge(width=0.2),color="black",width=0.1)+
    geom_boxplot(position=position_dodge(width =0.2),
                 width=0.4,outlier.alpha=0,color="black")+
    geom_line(aes(group=paired),position = position_dodge(0.2),color="grey80",alpha=0.4) +
    geom_point(aes(fill=pc_poistion,group=paired),pch=21,alpha = 0.8,size =1.5,
               position = position_dodge(0.2)) +
    scale_fill_manual(values = c("#FF0000","#00FF00","#0000FF")) +
    scale_color_manual(values = c("#4D4D4D","#4D4D4D","#4D4D4D")) +
    facet_wrap(.~season) +
    scale_x_discrete(guide = "prism_bracket")+
    labs(x=NULL,y=NULL)+
    theme_prism(base_line_size =0.5)+
    theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),units=,"cm"),
          strip.text = element_text(size=6),
          axis.line = element_line(color = "black",size = 0.4),
          panel.grid.minor = element_blank(),
          axis.text.y = element_text(color="black",size=6),
          axis.text.x = element_text(margin = margin(t = -5),color="black",size=6),
          legend.position = "none",
          plot.title = element_text(size = 8),
          panel.spacing = unit(0,"lines"))+
    coord_cartesian() +
    ggtitle(label) +
    theme(plot.margin = unit(c(0.1, 0.1, 0.1,0.1), "cm"))
  
}
plot_hist_brid = function(label,data){
  
  ## 构建绘图函数：
  clean_plot_temp1 <- data %>%
    dplyr::select("season","site_type",label) %>% rename(pc_poistion = site_type,label = label) %>% 
    filter(season == "Winter") %>% 
    mutate(paired = rep(1:(n()/2),each=2),pc_poistion=factor(pc_poistion)) %>% 
    mutate(pc_poistion = factor(pc_poistion,levels = c("PA-NPA","CA")))
  
  clean_plot_temp2 <- data %>%
    dplyr::select("season","site_type",label) %>% rename(pc_poistion = site_type,label = label) %>% 
    filter(season == "Summer") %>% 
    mutate(paired = rep(1:(n()/2),each=2),pc_poistion=factor(pc_poistion)) %>% 
    mutate(pc_poistion = factor(pc_poistion,levels = c("PA-NPA","CA")))
  
  clean_plot_temp = rbind(clean_plot_temp1,clean_plot_temp2) %>% data.frame() %>% 
    mutate(label = as.numeric(label))
  
  clean_plot_temp %>%
    ggplot(aes(pc_poistion,label,color = pc_poistion)) +
    stat_boxplot(geom="errorbar",position=position_dodge(width=0.2),color="black",width=0.1)+
    geom_boxplot(position=position_dodge(width =0.2),
                 width=0.4,outlier.alpha=0,color="black")+
    geom_line(aes(group=paired),position = position_dodge(0.2),color="grey80",alpha=0.4) +
    geom_point(aes(fill=pc_poistion,group=paired),pch=21,alpha = 0.8,size =1.5,
               position = position_dodge(0.2)) +
    scale_fill_manual(values = c("#FF0000","#0000FF")) +
    scale_color_manual(values = c("#4D4D4D","#4D4D4D")) +
    facet_wrap(.~season) +
    scale_x_discrete(guide = "prism_bracket")+
    labs(x=NULL,y=NULL)+
    theme_prism(base_line_size =0.5)+
    theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),units=,"cm"),
          strip.text = element_text(size=6),
          axis.line = element_line(color = "black",size = 0.4),
          panel.grid.minor = element_blank(),
          axis.text.y = element_text(color="black",size=6),
          axis.text.x = element_text(margin = margin(t = -5),color="black",size=6),
          legend.position = "none",
          plot.title = element_text(size = 8),
          text = element_text(family = "Times New Roman"), 
          panel.spacing = unit(0,"lines"))+
    coord_cartesian() +
    ggtitle(label) +
    theme(plot.margin = unit(c(0.1, 0.1, 0.1,0.1), "cm"))
}

## plot land_survey_data: 
plot_list_name = names(land_survey_data)[-c(1:5)]
plot_list = lapply(plot_list_name, plot_hist_land,data = land_survey_data)
lapply(1:length(plot_list_name), function(x){
  path = "./final_pv_survey_result/land_survey_plot_desc"
  tiff(paste0(path,"_",plot_list_name[x],".tiff"),width = 600,height = 1200,res=300)
  print(plot_list[[x]]) 
  dev.off()})

plot_list_name = names(brid_union_data)[-c(1:4)]
plot_list = lapply(plot_list_name, plot_hist_brid,data = brid_union_data)
lapply(1:length(plot_list_name), function(x){
  path = "./final_pv_survey_result/brid_survey_plot_desc/"
  tiff(paste0(path,"_",plot_list_name[x],".tiff"),width = 600,height = 1200,res=300)
  print(plot_list[[x]]) 
  dev.off()})

## 3.3.2 pari-test ----
## Aim: Using Wilcoxon nonparametric ANOVAs to compare the differences among PA-NPA, PA-CA, and NPA-CA groups in two seasons.
## function -wilo test : 
wilo = function(test_data){
  if(!any(grepl("PA-NPA", test_data$site_type))){
    test_data_a = test_data %>% filter(site_type  == "PA") %>% select(!site_type) %>% 
      mutate(across(everything(), as.numeric))
    test_data_b = test_data %>% filter(site_type  == "NPA") %>% select(!site_type) %>% 
      mutate(across(everything(), as.numeric))
    test_data_c = test_data %>% filter(site_type  == "CA")%>% select(!site_type) %>% 
      mutate(across(everything(), as.numeric))
    lapply(1:length(names(test_data_a)), function(x){
      result_ab <- wilcox.test(test_data_a[,x], test_data_b[,x],paired = TRUE)
      result_ac <- wilcox.test(test_data_a[,x], test_data_c[,x],paired = TRUE)
      result_bc <- wilcox.test(test_data_b[,x], test_data_c[,x],paired = TRUE)
      out = list()
      out$wilcox_test_PA_NPA = result_ab$p.value
      out$wilcox_test_PA_CA = result_ac$p.value
      out$wilcox_test_NPA_CA = result_bc$p.value
      out %>% do.call(rbind,.) %>% t() %>% data.frame()     
    })
  }else{
    test_data_a = test_data %>% filter(site_type  == "PA-NPA") %>% select(!site_type) %>% 
      mutate(across(everything(), as.numeric))
    test_data_c = test_data %>% filter(site_type  == "CA")%>% select(!site_type) %>% 
      mutate(across(everything(), as.numeric))
    lapply(1:length(names(test_data_a)), function(x){
      result_ab <- wilcox.test(test_data_a[,x], test_data_c[,x],paired = TRUE)
      out = list()
      out$wilcox_test_PA_NPA_CA  = result_ab$p.value
      out %>% do.call(rbind,.) %>% t() %>% data.frame()     
    })
  }
}

## stat land_survey_data: 
land_wilotest_result = rbind(land_survey_data %>%  filter(season == "Winter") %>% 
                               select(!c("season","pv_type","site")) %>% wilo(.) %>% 
                               do.call(rbind,.) %>% data.frame() %>% 
                               cbind(label = names(land_survey_data)[5:23],.) %>%
                               mutate(season = "winter"),
                             land_survey_data %>%  filter(season == "Summer") %>% 
                               select(!c("season","pv_type","site")) %>% wilo(.) %>% 
                               do.call(rbind,.) %>% data.frame() %>% 
                               cbind(label = names(land_survey_data)[5:23],.) %>% 
                               mutate(season = "summer")) 

## stat brid_survey_data: 
brid_wilotest_result = rbind(brid_union_data %>%  filter(season == "Winter") %>% 
                          select(!c("season","pv_type","site")) %>% wilo(.) %>% 
                          do.call(rbind,.) %>% data.frame() %>% 
                          cbind(label = names(brid_union_data)[5:8],.) %>%
                          mutate(season = "winter"),
                          brid_union_data %>%  filter(season == "Summer") %>% 
                          select(!c("season","pv_type","site")) %>% wilo(.) %>% 
                          do.call(rbind,.) %>% data.frame() %>% 
                          cbind(label = names(brid_union_data)[5:8],.) %>% 
                          mutate(season = "summer")) 

wilotest_result =list(land_wilotest_result,brid_wilotest_result)
names(wilotest_result) = c("land_wilotest_result","brid_wilotest_result")

writexl::write_xlsx(wilotest_result, "./final_pv_survey_result/wilotest_result_total.xlsx")

## 3.3.3 pari-test-pv-type -----

## stat land_survey_data: 
land_wilotest_result_pv = rbind(land_survey_data %>%  filter(season == "Winter") %>% 
                               filter(pv_type  == "FPV") %>% 
                               select(!c("season","pv_type","site")) %>% wilo(.) %>% 
                               do.call(rbind,.) %>% data.frame() %>% 
                               cbind(label = names(land_survey_data)[5:23],.) %>%
                               mutate(season = "winter",pv_type  = "FPV"),
                             land_survey_data %>%  filter(season == "Winter") %>% 
                               filter(pv_type  == "PMPV") %>% 
                               select(!c("season","pv_type","site")) %>% wilo(.) %>% 
                               do.call(rbind,.) %>% data.frame() %>% 
                               cbind(label = names(land_survey_data)[5:23],.) %>%
                              mutate(season = "winter",pv_type  = "PMPV"),
                             land_survey_data %>%  filter(season == "Summer") %>% 
                               filter(pv_type  == "FPV") %>% 
                               select(!c("season","pv_type","site")) %>% wilo(.) %>% 
                               do.call(rbind,.) %>% data.frame() %>% 
                               cbind(label = names(land_survey_data)[5:23],.) %>% 
                               mutate(season = "summer",pv_type  = "FPV"),
                             land_survey_data %>%  filter(season == "Summer") %>% 
                               filter(pv_type  == "PMPV") %>% 
                               select(!c("season","pv_type","site")) %>% wilo(.) %>% 
                               do.call(rbind,.) %>% data.frame() %>% 
                               cbind(label = names(land_survey_data)[5:23],.) %>% 
                               mutate(season = "summer",pv_type  = "PMPV")) 

## stat brid_survey_data: 
brid_wilotest_result_pv = rbind(brid_union_data %>%  filter(season == "Winter") %>% 
                               filter(pv_type  == "FPV") %>% 
                               select(!c("season","pv_type","site")) %>% wilo(.) %>% 
                               do.call(rbind,.) %>% data.frame() %>% 
                               cbind(label = names(brid_union_data)[5:8],.) %>%
                               mutate(season = "winter",pv_type  = "FPV"),
                             brid_union_data %>%  filter(season == "Winter") %>% 
                               filter(pv_type  == "PMPV") %>% 
                               select(!c("season","pv_type","site")) %>% wilo(.) %>% 
                               do.call(rbind,.) %>% data.frame() %>% 
                               cbind(label = names(brid_union_data)[5:8],.) %>%
                               mutate(season = "winter",pv_type  = "PMPV"),
                             brid_union_data %>%  filter(season == "Summer") %>%
                               filter(pv_type  == "FPV") %>% 
                               select(!c("season","pv_type","site")) %>% wilo(.) %>% 
                               do.call(rbind,.) %>% data.frame() %>% 
                               cbind(label = names(brid_union_data)[5:8],.) %>% 
                               mutate(season = "summer",pv_type  = "FPV"),
                             brid_union_data %>%  filter(season == "Summer") %>% 
                               filter(pv_type  == "PMPV") %>% 
                               select(!c("season","pv_type","site")) %>% wilo(.) %>% 
                               do.call(rbind,.) %>% data.frame() %>% 
                               cbind(label = names(brid_union_data)[5:8],.) %>% 
                               mutate(season = "summer",pv_type  = "PMPV")) 

wilotest_result =list(land_wilotest_result_pv,brid_wilotest_result_pv)
names(wilotest_result) = c("land_wilotest_result_pv","brid_wilotest_result_pv")

writexl::write_xlsx(wilotest_result, "./final_pv_survey_result/wilotest_result_pv.xlsx")

## 3.4 
## 
## 3.4 stat：Community difference(NMDS) ----
## function:
nmds_stress_calcu= function(data_clean){
  nmds_data_ws = data_clean %>% rename(pc_poistion = site_location) %>% 
    transform(pc_poistion =as.factor(pc_poistion))
  
  nmds_otu_ws = nmds_data_ws %>% dplyr::select(!pc_poistion) %>%
    mutate(across(.cols =everything(),.fns =as.numeric))
  otu.distance <- vegdist(nmds_otu_ws, method = 'bray')
  
  if(length(unique(nmds_data_ws$pc_poistion)) ==2){
    meta_nmds <- metaMDS(otu.distance, k = 2)
  }else{ meta_nmds <- metaMDS(otu.distance, k = 3)}
  
  stress_calcu = list()
  
  stress_calcu$stress_value =  meta_nmds$stress

  stress_calcu$meta_nmds = meta_nmds

  stress_calcu$group = nmds_data_ws$pc_poistion
  
  stress_calcu$distance = otu.distance
  
  stress_calcu$pc_poistion.anosim <- anosim(otu.distance, nmds_data_ws$pc_poistion)
  
  return(stress_calcu)
}
gg_nmds_plot = function(nmds_stress_calcu){
  
  nmds_points <- as.data.frame(nmds_stress_calcu$meta_nmds$points)
  nmds_points$samples <- row.names(nmds_points)
  names(nmds_points)[1:2] <- c('NMDS1', 'NMDS2')
  
  gg_nmds = cbind(group = nmds_stress_calcu$group,nmds_points) %>%  data.frame() %>% 
    mutate(group = factor(group, levels = c("PA", "NPA", "CA")))
  
  mycolors=c("#FF0000","#00FF00","#0000FF")
  ggplot(data=gg_nmds,aes(x=NMDS1,y=NMDS2))+
    geom_point(aes(fill = factor(group),shape = factor(group)),
               size=1.5)+
    scale_fill_manual(values=mycolors) +
    scale_shape_manual(values = c(21:25))+
    geom_hline(yintercept=0,linetype=3,size=1,alpha = 0.5) +
    geom_vline(xintercept=0,linetype=3,size=1,alpha = 0.5) +
    stat_ellipse(data=gg_nmds,
                 level=0.95,
                 linetype = 2,size=0.5,
                 aes(color=group),
                 alpha=0.8)+
    scale_color_manual(values = mycolors) +
    guides(color = FALSE) +
    scale_fill_manual(values = mycolors)+
    ggtitle(paste('Stress=',round(nmds_stress_calcu$stress_value, 3))) +
    theme_bw(base_family = "Times New Roman") +
    theme(panel.grid=element_blank(),
          axis.title.y=element_text(angle=90)) +
    theme(legend.position = "none")
  
}
gg_nmds_plot_brid = function(nmds_stress_calcu){
  
  nmds_points <- as.data.frame(nmds_stress_calcu$meta_nmds$points)
  nmds_points$samples <- row.names(nmds_points)
  names(nmds_points)[1:2] <- c('NMDS1', 'NMDS2')
  
  gg_nmds = cbind(group = nmds_stress_calcu$group,nmds_points) %>%  data.frame() 

  mycolors=c("#FF0000","#0000FF")
  ggplot(data=gg_nmds,aes(x=NMDS1,y=NMDS2))+
    geom_point(aes(fill = factor(group),shape = factor(group)),
               size=1.5)+
    scale_fill_manual(values=mycolors) +
    scale_shape_manual(values = c(21,23))+
    geom_hline(yintercept=0,linetype=3,size=1,alpha = 0.5) +
    geom_vline(xintercept=0,linetype=3,size=1,alpha = 0.5) +
    stat_ellipse(data=gg_nmds,
                 level=0.95,
                 linetype = 2,size=0.5,
                 aes(color=group),
                 alpha=0.8)+
    scale_color_manual(values = mycolors) +
    guides(color = FALSE) +
    ggtitle(paste('Stress=',round(nmds_stress_calcu$stress_value, 3))) +
    theme_bw(base_family = "Times New Roman") +
    theme(panel.grid=element_blank(),
          axis.title.y=element_text(angle=90)) +
    theme(legend.position = "none")
  
} 

## plot nmds:
land_survey_data_name <- 
  c("phytoplankton_winter", "phytoplankton_summer",
    "rotifers_Winter", "rotifers_summmer",
    "zooplankton_winer", "zooplankton_summer")
brid_survey_data_name <- 
  c("bird_ecotypes_number_winter", "bird_ecotypes_number_summer",
    "bird_ecotype_density_winter","bird_ecotype_density_summer")
      
brid_survey_data_name2 <- 
  c("brid_number_winter", "brid_number_summer",
    "brid_density_winter","brid_density_summer")

lapply(land_survey_data_name, function(text_data){
    # text_data =  "phytoplankton_winter"
    path = "./final_pv_survey_result/NMDS/"
    file_name = paste0(path,text_data,".tiff")
    tiff(file_name,width = 800,height = 800,res=300)
    print(gg_nmds_plot(nmds_stress_calcu(get(text_data)  %>%
                                   select(!site))))
    dev.off()
    
  })

lapply(brid_survey_data_name, function(text_data){
  path = "./final_pv_survey_result/NMDS/"
  file_name = paste0(path,text_data,".tiff")
  tiff(file_name,width = 800,height = 800,res=300)
  print(gg_nmds_plot_brid(nmds_stress_calcu(
    get(text_data)[,-1])))
  dev.off()
})

lapply(brid_survey_data_name2, function(text_data){
  path = "./final_pv_survey_result/NMDS/"
  file_name = paste0(path,text_data,".tiff")
  tiff(file_name,width = 800,height = 800,res=300)
  print(gg_nmds_plot_brid(nmds_stress_calcu(
    get(text_data)[,-1])))
  dev.off()
})

## 3.5 stat：community similarity(ANOSIM)------
# Analysis of similarity (ANOSIM) of phytoplankton, microzooplankton, and macrozooplankton communities among three area;
# function:
land_survey_data_name <- 
  c("phytoplankton_winter", "phytoplankton_summer",
    "rotifers_Winter", "rotifers_summmer",
    "zooplankton_winer", "zooplankton_summer")
brid_survey_data_name <- 
  c("bird_ecotypes_number_winter", "bird_ecotypes_number_summer",
    "bird_ecotype_density_winter","bird_ecotype_density_summer")

brid_survey_data_name <- 
  c("brid_number_winter", "brid_number_summer",
    "brid_density_winter","brid_density_summer")

anosim_calc = function(aim_data){
  aim_data = phytoplankton_winter
  nmds_data_clean = aim_data %>% 
    dplyr::select(!site) %>%
    rename(pc_poistion = site_location) %>% 
    transform(pc_poistion =as.factor(pc_poistion))
  
  nmds_data_clean_ab = nmds_data_clean %>% 
    filter(pc_poistion == "PA" | pc_poistion == "NPA")
  
  nmds_data_clean_ac = nmds_data_clean %>% 
    filter(pc_poistion == "PA" | pc_poistion == "CA")
  
  nmds_data_clean_bc = nmds_data_clean %>% 
    filter(pc_poistion == "NPA" | pc_poistion == "CA")
  
  
  anosim_result = function(union_nmds_data_clean){
    nmds_otu_ws = union_nmds_data_clean %>% 
      dplyr::select(all_of(names(union_nmds_data_clean)[-1])) %>% 
      mutate(across(.cols =everything(),.fns =as.numeric)) 
    otu.distance <- vegdist(nmds_otu_ws, method = 'bray')

    pc_poistion.anosim <- anosim(otu.distance, union_nmds_data_clean$pc_poistion) 
    result = data.frame(
      ANOSIM_statistic_R = pc_poistion.anosim$statistic,
      ANOSIM_statistic_Significance = pc_poistion.anosim$signif)
    result
  }
  abc = rbind(anosim_result(nmds_data_clean_ab),
              anosim_result(nmds_data_clean_ac),
              anosim_result(nmds_data_clean_bc)) %>% data.frame() %>% 
    mutate(class = c("PA-NPA","PA-CA","NPA-CA"))
  
}
anosim_calc_bd = function(aim_data){
  nmds_data_clean = aim_data[,-1] %>%
    rename(pc_poistion = site_location) %>% 
    transform(pc_poistion =as.factor(pc_poistion))

  nmds_data_clean_ac = nmds_data_clean %>% 
    filter(pc_poistion == "PA-NPA" | pc_poistion == "CA")
  
  anosim_result = function(nmds_data_clean_data){
    union_nmds_data_clean = nmds_data_clean_data

    nmds_otu_ws = union_nmds_data_clean %>% 
      dplyr::select(all_of(names(union_nmds_data_clean)[-1])) %>% 
      mutate(across(.cols =everything(),.fns =as.numeric)) %>% 
      select_if(~ sum(.) > 0)
    
    otu.distance <- vegdist(nmds_otu_ws, method = 'bray', zeroes = TRUE)
    pc_poistion.anosim <- anosim(otu.distance, union_nmds_data_clean$pc_poistion) 
    result = data.frame(
      ANOSIM_statistic_R =  ifelse(length(pc_poistion.anosim$statistic) == 0, 
                                   "ANOISM statistics are calculated as 0", pc_poistion.anosim$statistic),
      ANOSIM_statistic_Significance = pc_poistion.anosim$signif)
    result
  }
  ac = anosim_result(nmds_data_clean_ac)
  ac        
 
}

land_survey_anosim = 
  lapply(land_survey_data_name,function(text_data){
  anosim_calc(get(text_data)) %>% 
    cbind(label = text_data,.)
  }) %>% do.call(rbind,.) %>% data.frame()

brid_survey_anosim = 
  lapply(brid_survey_data_name,function(text_data){
    anosim_calc_bd(get(text_data)) %>%
      cbind(label = text_data,.)
  }) %>% do.call(rbind,.) %>% data.frame() %>% 
  mutate(class = "PA-NPA-CA")

brid_survey_anosim2 = 
  lapply(brid_survey_data_name,function(text_data){
    anosim_calc_bd(get(text_data)) %>%
      cbind(label = text_data,.)
  }) %>% do.call(rbind,.) %>% data.frame() %>% 
  mutate(class = "PA-NPA-CA")

survey_anosim_result = rbind(land_survey_anosim,brid_survey_anosim,brid_survey_anosim2)

writexl::write_xlsx(survey_anosim_result, "./final_pv_survey_result/survey_anosim_result.xlsx")
  
## 3.6 stat：Relationship between environmental factors and biological indicators（RDA）------
# When calculating RDA, convert the density data into log10.
# function:
## Extract modeling data：
aim_data_union = function(data_season,select_dsh = "phyto"){
  model_data_test = data_season
  model_data_res = model_data_test %>% 
    mutate(d_macrozo =as.numeric(d_macrozo)) %>% 
    mutate(d_microzo =as.numeric(d_microzo)) %>% 
    mutate(d_phyto =as.numeric(d_phyto)) %>% 
    mutate(d_macrozo = log10(d_macrozo),
           d_microzo = log10(d_microzo),
           d_phyto = log10(d_phyto)) %>% 
    dplyr::select(contains(select_dsh)) %>% 
    select(!contains("E")) %>% 
    data.frame() 
  
  spe.hel = model_data_res
  
  model_data_exp = model_data_test %>% dplyr::select(
    c("DNI", "pH", "T", "DO", "TN", "TP", "NH4"))
  
  out = list()
  out$model_data_test = model_data_test
  out$spe.hel = spe.hel
  out$model_data_exp = model_data_exp
  out
  
}
## Model construction and description result extraction：
rda_extract = function(extract_data){
  # extract_data = aim_data_all
  spe.hel  = extract_data$spe.hel %>% 
    mutate(across(.cols =everything(),.fns =as.numeric))
  model_data_exp = extract_data$model_data_exp %>% 
    mutate(across(.cols =everything(),.fns =as.numeric))
  
  # 关注省略模式的公式
  # 这里使用一些默认的选项，即 scale=FALSE（基于协方差矩阵的RDA）和#scaling=2
  spe.rda <- rda(spe.hel,model_data_exp,scale=TRUE) 
  exp_all = summary(spe.rda)
  exp = exp_all$cont$importance %>% data.frame() %>% 
    dplyr::select(c("RDA1","RDA2")) %>% 
    filter(row_number() ==2)
  # ordiplot(exp_all, choices = c(1, 2))
  # 从rda的结果中提取校正R2
  adjust_R2 = (R2adj <- RsquareAdj(spe.rda)$adj.r.squared)  # 0.129932
  
  # RDA所有轴置换检验
  # install.packages("broom")
  library(broom)
  RDA_Permutation_test = anova.cca(spe.rda, step=1000) %>% broom::tidy() %>% 
    dplyr::select(p.value) %>% .$p.value %>% .[1]
  
  out = list()
  out$exp_all = exp_all
  out$exp = exp
  out$adjust_R2 = adjust_R2
  out$RDA_Permutation_test =RDA_Permutation_test
  out
}
## plot rda ：
rda_mult = function(extract_data,sum_rda){
  
  model_data_test = extract_data$model_data_test
  
  sp=as.data.frame(sum_rda$exp_all$species[,1:2]) *3 
  st=as.data.frame(sum_rda$exp_all$sites[,1:2])  
  yz=as.data.frame(sum_rda$exp_all$biplot[,1:2]) *3 
  
  class = factor(model_data_test %>% .$site_type,levels = c("PA","NPA","CA"))

  mycolors<-c("#FF0000","#00FF00","#0000FF")
  ggplot() +
    geom_point(data = st,aes(RDA1,RDA2,fill = factor(class),
                             shape = factor(class)),size=1.5)+
    scale_fill_manual(values=mycolors) +
    scale_shape_manual(values = c(21:25))+
    geom_segment(data = sp,aes(x = 0, y = 0, xend = RDA1, yend = RDA2),
                 arrow = arrow(angle=22.5,length = unit(0.35,"cm"),
                               type = "closed"),linetype=1, size=0.6,
                 colour = "#FF0000")+
    geom_text_repel(data = sp,aes(RDA1,RDA2,label=row.names(sp)),
                    hjust =1)+
    geom_segment(data = yz,aes(x = 0, y = 0, xend = RDA1, yend = RDA2), 
                 arrow = arrow(angle=22.5,length = unit(0.35,"cm"),
                               type = "closed"),linetype=1, size=0.6,
                 colour = "#0000FF")+
    geom_text_repel(data = yz,aes(RDA1,RDA2,label=row.names(yz)),
                    force = 1,hjust =0.5)+
    labs(x=paste("RDA1"," ",round(sum_rda$exp$RDA1*100,2),"%"),
         y = paste("RDA2"," ",round(sum_rda$exp$RDA2*100,2),"%"))+ 
    geom_hline(yintercept=0,linetype=3,size=1) +
    geom_vline(xintercept=0,linetype=3,size=1) +
    theme_bw()+ theme(axis.title.x=element_text(size=12),
                      axis.title.y=element_text(size=12,angle=90),
                      axis.text.y=element_text(size=10),
                      axis.text.x=element_text(size=10),
                      panel.grid=element_blank())+
    ggtitle(paste0("Adjust_R2=",round(sum_rda$adjust_R2,2),
                   "Permutation_test=",sum_rda$RDA_Permutation_test)) +
    theme(legend.position = "none",plot.title = element_text(size = 10))
  
}
rda_mult_no_label = function(extract_data,sum_rda){
  model_data_test = extract_data$model_data_test
  
  sp=as.data.frame(sum_rda$exp_all$species[,1:2]) *3 
  st=as.data.frame(sum_rda$exp_all$sites[,1:2])  
  yz=as.data.frame(sum_rda$exp_all$biplot[,1:2]) *3 
  
  class = factor(model_data_test %>% .$site_type,levels = c("PA","NPA","CA"))
  
  mycolors<-c("#FF0000","#00FF00","#0000FF")
  ggplot() +
    geom_point(data = st,aes(RDA1,RDA2,fill = factor(class),
                             shape = factor(class)),size=1.5)+
    scale_fill_manual(values=mycolors) +
    scale_shape_manual(values = c(21:25))+
    geom_segment(data = sp,aes(x = 0, y = 0, xend = RDA1, yend = RDA2),
                 arrow = arrow(angle=22.5,length = unit(0.35,"cm"),
                               type = "closed"),linetype=1, size=0.6,
                 colour = "#FF0000")+
    geom_segment(data = yz,aes(x = 0, y = 0, xend = RDA1, yend = RDA2), 
                 arrow = arrow(angle=22.5,length = unit(0.35,"cm"),
                               type = "closed"),linetype=1, size=0.6,
                 colour = "#0000FF")+
    labs(x=paste("RDA1"," ",round(sum_rda$exp$RDA1*100,2),"%"),
         y = paste("RDA2"," ",round(sum_rda$exp$RDA2*100,2),"%"))+ 
    geom_hline(yintercept=0,linetype=3,size=1) +
    geom_vline(xintercept=0,linetype=3,size=1) +
    theme_bw()+ theme(axis.title.x=element_text(size=12),
                      axis.title.y=element_text(size=12,angle=90),
                      axis.text.y=element_text(size=10),
                      axis.text.x=element_text(size=10),
                      panel.grid=element_blank())+
    ggtitle(paste0("Adjust_R2=",round(sum_rda$adjust_R2,2),
                   "Permutation_test=",sum_rda$RDA_Permutation_test)) +
    theme(legend.position = "none",plot.title = element_text(size = 10))
  
}

## 修改图形的边距（20240807）：
rda_mult_center = function(extract_data,sum_rda){
  
  model_data_test = extract_data$model_data_test
  
  sp=as.data.frame(sum_rda$exp_all$species[,1:2]) *3 
  st=as.data.frame(sum_rda$exp_all$sites[,1:2])  
  yz=as.data.frame(sum_rda$exp_all$biplot[,1:2]) *3 
  
  class = factor(model_data_test %>% .$site_type,levels = c("PA","NPA","CA"))
  
  mycolors<-c("#FF0000","#00FF00","#0000FF")
  ggplot() +
    geom_point(data = st,aes(RDA1,RDA2,fill = factor(class),
                             shape = factor(class)),size=1.5)+
    scale_fill_manual(values=mycolors) +
    scale_shape_manual(values = c(21:25))+
    geom_segment(data = sp,aes(x = 0, y = 0, xend = RDA1, yend = RDA2),
                 arrow = arrow(angle=22.5,length = unit(0.35,"cm"),
                               type = "closed"),linetype=1, size=0.6,
                 colour = "#FF0000")+
    geom_text_repel(data = sp,aes(RDA1,RDA2,label=row.names(sp)),
                    hjust =1)+
    geom_segment(data = yz,aes(x = 0, y = 0, xend = RDA1, yend = RDA2), 
                 arrow = arrow(angle=22.5,length = unit(0.35,"cm"),
                               type = "closed"),linetype=1, size=0.6,
                 colour = "#0000FF")+
    geom_text_repel(data = yz,aes(RDA1,RDA2,label=row.names(yz)),
                    force = 1,hjust =0.5)+
    xlim(-4, 4) +  # 设置x轴范围为-4到4
    ylim(-5, 5) +   # 设置y轴范围为-5到5
    labs(x=paste("RDA1"," ",round(sum_rda$exp$RDA1*100,2),"%"),
         y = paste("RDA2"," ",round(sum_rda$exp$RDA2*100,2),"%"))+ 
    geom_hline(yintercept=0,linetype=3,size=1) +
    geom_vline(xintercept=0,linetype=3,size=1) +
    theme_bw()+ theme(axis.title.x=element_text(size=12),
                      axis.title.y=element_text(size=12,angle=90),
                      axis.text.y=element_text(size=10),
                      axis.text.x=element_text(size=10),
                      panel.grid=element_blank())+
    ggtitle(paste0("Adjust_R2=",round(sum_rda$adjust_R2,2),
                   "Permutation_test=",sum_rda$RDA_Permutation_test)) +
    theme(legend.position = "none",plot.title = element_text(size = 10))
  
}
rda_mult_no_label_center = function(extract_data,sum_rda){
  model_data_test = extract_data$model_data_test
  
  sp=as.data.frame(sum_rda$exp_all$species[,1:2]) *3 
  st=as.data.frame(sum_rda$exp_all$sites[,1:2])  
  yz=as.data.frame(sum_rda$exp_all$biplot[,1:2]) *3 
  
  class = factor(model_data_test %>% .$site_type,levels = c("PA","NPA","CA"))
  
  mycolors<-c("#FF0000","#00FF00","#0000FF")
  ggplot() +
    geom_point(data = st,aes(RDA1,RDA2,fill = factor(class),
                             shape = factor(class)),size=1.5)+
    scale_fill_manual(values=mycolors) +
    scale_shape_manual(values = c(21:25))+
    geom_segment(data = sp,aes(x = 0, y = 0, xend = RDA1, yend = RDA2),
                 arrow = arrow(angle=22.5,length = unit(0.35,"cm"),
                               type = "closed"),linetype=1, size=0.6,
                 colour = "#FF0000")+
    geom_segment(data = yz,aes(x = 0, y = 0, xend = RDA1, yend = RDA2), 
                 arrow = arrow(angle=22.5,length = unit(0.35,"cm"),
                               type = "closed"),linetype=1, size=0.6,
                 colour = "#0000FF")+
    labs(x=paste("RDA1"," ",round(sum_rda$exp$RDA1*100,2),"%"),
         y = paste("RDA2"," ",round(sum_rda$exp$RDA2*100,2),"%"))+ 
    geom_hline(yintercept=0,linetype=3,size=1) +
    geom_vline(xintercept=0,linetype=3,size=1) +
    theme_bw()+ theme(axis.title.x=element_text(size=12),
                      axis.title.y=element_text(size=12,angle=90),
                      axis.text.y=element_text(size=10),
                      axis.text.x=element_text(size=10),
                      panel.grid=element_blank())+
    xlim(-4, 4) +  # 设置x轴范围为-4到4
    ylim(-5, 5) +   # 设置y轴范围为-5到5
    ggtitle(paste0("Adjust_R2=",round(sum_rda$adjust_R2,2),
                   "Permutation_test=",sum_rda$RDA_Permutation_test)) +
    theme(legend.position = "none",plot.title = element_text(size = 10))
  
}

land_survey_data_Winter = land_survey_data %>% 
  filter(season == "Winter") %>% arrange(site_type)
land_survey_data_Summer = land_survey_data %>% 
  filter(season == "Summer") %>% arrange(site_type)

bio_text = c("micr","macr","phyto")

# land_survey_data_Winter_rda
lapply(bio_text, function(text){
  # text = bio_text[1]
  aim_data_all = aim_data_union(land_survey_data_Winter,select_dsh = text)
  sum_rda = rda_extract(aim_data_all)
  
  path = "./final_pv_survey_result/RDA/RDA-label"
  file_name = paste0(path,"Winter_",text,".tiff")
  tiff(file_name,
       width = 1200,height = 1200,res=300)
  print(rda_mult(aim_data_all,sum_rda))
  dev.off()
})

# land_survey_data_Summer_rda 
lapply(bio_text, function(text){
  aim_data_all = aim_data_union(land_survey_data_Summer,select_dsh = text)
  sum_rda = rda_extract(aim_data_all)
  path = "./final_pv_survey_result/RDA/RDA-label"
  file_name = paste0(path,"Summer_",text,".png")
  tiff(file_name,
       width = 1200,height = 1200,res=300)
  print(rda_mult(aim_data_all,sum_rda))
  dev.off()
})

# land_survey_data_Winter_rda
lapply(bio_text, function(text){
  # text = bio_text[1]
  aim_data_all = aim_data_union(land_survey_data_Winter,select_dsh = text)
  rda_mult_no_label(aim_data_all,sum_rda)
  path = "./final_pv_survey_result/RDA/"
  file_name = paste0(path,"Winter_no_label",text,".png")
  tiff(file_name,
       width = 1200,height = 1200,res=300)
  print(rda_mult_no_label(aim_data_all,sum_rda))
  dev.off()
})

# land_survey_data_Summer_rda 
lapply(bio_text, function(text){
  aim_data_all = aim_data_union(land_survey_data_Summer,select_dsh = text)
  sum_rda = rda_extract(aim_data_all)
  path = "./final_pv_survey_result/RDA/"
  file_name = paste0(path,"Summer_no_label",text,".png")
  tiff(file_name,
       width = 1200,height = 1200,res=300)
  print(rda_mult_no_label(aim_data_all,sum_rda))
  dev.off()
})

## 3.7 stat：Correlation between environmental and biological indicators（Spearman）-----
## function:
Spearman_plot = function(model_data_test){

  model_data_res = model_data_test %>% 
    select(all_of(c("s_phyto","d_phyto",
                    "s_microzo","d_microzo","s_macrozo","d_macrozo"))) %>% 
    mutate(across(everything(), as.numeric)) 
    
  
  model_data_exp = model_data_test %>% dplyr::select(
    c("DNI", "pH", "T", "DO", "TN", "TP", "NH4",
      "s_phyto","d_phyto","s_microzo","d_microzo")) %>% 
    mutate(across(everything(), as.numeric))
  
  cor_data = cor(model_data_res,model_data_exp,use="everything",method= c("spearman"))
  library(ggcorrplot)
  ggcorrplot(cor_data)
}
Spearman_plot2 = function(model_data_test){
  
  model_data_res = model_data_test %>% 
    select(all_of(c("s_phyto","d_phyto","H_phyto",
                    "s_microzo","d_microzo","H_microzo",
                    "s_macrozo","d_macrozo","H_macrozo"))) %>% 
    mutate(across(everything(), as.numeric)) 
  
  
  model_data_exp = model_data_test %>% dplyr::select(
    c("DNI", "pH", "T", "DO", "TN", "TP", "NH4",
      "s_phyto","d_phyto","s_microzo","d_microzo")) %>% 
    mutate(across(everything(), as.numeric))
  
  cor_data = cor(model_data_res,model_data_exp,use="everything",method= c("spearman"))
  library(ggcorrplot)
  ggcorrplot(cor_data)
}
land_survey_data_Winter = land_survey_data %>% 
  filter(season == "Winter") %>% arrange(site_type)
land_survey_data_Summer = land_survey_data %>% 
  filter(season == "Summer") %>% arrange(site_type)

env_name = c("land_survey_data_Winter","land_survey_data_Summer")
lapply(env_name, function(text){
  path = "./final_pv_survey_result/Spearman/"
  file_name = paste0(path,text,".tiff")
  Spearman_plot(get(text))
  ggsave(file_name)
})
lapply(env_name, function(text){
  path = "./final_pv_survey_result/Spearman/"
  file_name = paste0(path,text,"_H_.tiff")
  Spearman_plot2(get(text))
  ggsave(file_name)
})

library(GGally)

ggpairs(land_survey_data_Winter[,-c(1:4)] %>% 
          mutate(across(everything(), as.numeric),wrap = c(method = "spearman")))
  theme_minimal() +
  labs(title = "Spearman Correlation and Significance Matrix")
  
ggpairs(land_survey_data_Summer[,-c(1:4)] %>% 
            mutate(across(everything(), as.numeric),wrap = c(method = "spearman")))
  theme_minimal() +
    labs(title = "Spearman Correlation and Significance Matrix")
