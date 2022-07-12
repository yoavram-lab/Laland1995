################################################################
###########     Generating data for Table 1 ####################
################################################################
#
#
#  For computing the various family categories presented in Table 1,
#  a full pedigree was required, i.e. including some mock individuals.
#  For example, a childless couple cannot be recognized as a couple, as
#  there is not link between them in the pedigree; thus a mock child is
#  added in that case, creating a link between them. The list of all added
#  mock individuals is in the mock_individuals.RData file. Sampled individuals
#  (ego, or focal) are identified in the file ego_individuals.RData. All 
#  other categories (parents, grand-parents, child,sibs,  spouse and 
#  spouse's familiy) are identified topologically in the pedigrees, from ego. 

    rm(list=ls())

    load("full_pedigree_for_Table_1.RData")
    load("mock_individuals.RData")
    load("ego_individuals.RData")
    fami<-unique(fl2$famille)
    
################### Mothers and fathers of ego ####
    IDEgofather<-NULL
    IDEgomother<-NULL
    for(i in 1:length(fami)){
      ped<-fl2[which(fl2$famille==fami[i]),]
      IDego<- ped$animal[ped$animal%in%Allego$animal] #identification ego
      localfather<-ped$Idfather[which(ped$animal%in%IDego)]
      localmother<-ped$Idmother[which(ped$animal%in%IDego)]
      IDEgofather<-c(IDEgofather,localfather)
      IDEgomother<-c(IDEgomother,localmother)
    }
    father<-unique(IDEgofather)
    mother<-unique(IDEgomother)
    fathers<-fl2[which(fl2$animal %in%father),] 
    mothers<-fl2[which(fl2$animal %in%mother),] 
    mothers<-mothers[-which(mothers$animal%in%mock_individuals),]
    fathers<-fathers[-which(fathers$animal%in%mock_individuals),]
    parents<-rbind(fathers,mothers)
    mother_father<-parents$animal

################### Grand parents of ego       ####
    IDEgoGP<-NULL
    for(i in 1:length(fami)){
      ped<-fl2[which(fl2$famille==fami[i]),]
      mother_father_Ped<-ped$animal[which(ped$animal%in%mother_father)]
      localGP1<-ped$Idfather[which(ped$animal%in%mother_father_Ped)]
      localGP2<-ped$Idmother[which(ped$animal%in%mother_father_Ped)]
      IDEgoGP<-c(IDEgoGP,localGP1,localGP2)     
    }
    IDEgoGP<-unique(IDEgoGP)
    GP<-fl2[which(fl2$animal %in%IDEgoGP),] 
    GP1<-GP[-which(GP$animal %in%mock_individuals),] 
################### Child of ego               ####
    IDEgoChild<-NULL
    for(i in 1:length(fami)){
      ped<-fl2[which(fl2$famille==fami[i]),]
      IDego<- ped$animal[ped$animal%in%Allego$animal]
      localc1<-ped$animal[which(ped$Idfather%in% IDego)]
      localc2<-ped$animal[which(ped$Idmother%in% IDego)]
      IDEgoChild<-c(IDEgoChild,localc1,localc2)     
    }
    IDEgoChild<-unique(IDEgoChild)
    IDEgoChild<-setdiff(IDEgoChild,mock_individuals)
    child<-fl2[which(fl2$animal %in%IDEgoChild),]
    list_child<-child$animal#include mock child
################### Sibs of ego                ####
    IDEgoSibs<-NULL
    for(i in 1:length(fami)){
      ped<-fl2[which(fl2$famille==fami[i]),]
      mother_father_Ped<-ped$animal[which(ped$animal%in%mother_father)]
      IDego<- ped$animal[ped$anima%in%Allego$animal]
      localsib1<-ped$animal[which(ped$Idfather%in%mother_father_Ped)]
      localsib2<-ped$animal[which(ped$Idmother%in%mother_father_Ped)] # full and half sibs
      localsib<-unique(c(localsib1,localsib2))
      localsib<-setdiff(localsib,IDego) #removing ego from the sibs
      IDEgoSibs<-c(IDEgoSibs,localsib)     
    }
    IDEgoSibs<-unique(IDEgoSibs)
    sibs<-fl2[which(fl2$animal %in%IDEgoSibs),] 
    which(sibs$animal %in%mock_individuals)#none (as expected) 
################### Spouse and spouse family   ####
            # a spouse has a child with ego
            IDspouse<-NULL
            for(i in 1:length(fami)){
              ped<-fl2[which(fl2$famille==fami[i]),]
              IDego<- ped$animal[ped$anima%in%Allego$animal] #identification ego
              local1<-  ped$Idfather[which(ped$Idmother%in% IDego)]
              local2<-  ped$Idmother[which(ped$Idfather%in% IDego)]
              localspouse<-unique(c(local1,local2))
              localspouse<-setdiff(localspouse,IDego) #removing ego 
              IDspouse<-c(IDspouse,localspouse)     
            } 
            IDspouse<-unique(IDspouse)
            IDSpousefamily<-NULL
            for(i in 1:length(fami)){
              ped<-fl2[which(fl2$famille==fami[i]),]
              IDego<- ped$animal[ped$anima%in%Allego$animal] #identification ego
              spouse_ped<-ped$animal[which(ped$animal%in%IDspouse)]#identification spouse
              p1<-ped$Idfather[which(ped$animal%in%spouse_ped)] # parent
              p2<-ped$Idmother[which(ped$animal%in%spouse_ped)] # parent
              P<-unique(c(p1,p2))
              P<-P[!(is.na(P))]#removing NA
              gp1<-ped$Idfather[which(ped$animal%in%P)]#Grand parents
              gp2<-ped$Idmother[which(ped$animal%in%P)]
              GP<-unique(c(gp1,gp2))
              GP<-GP[!is.na(GP)]#removing NA
              gp1<-ped$Idfather[which(ped$animal%in%GP)]#Gran-grand parents
              gp2<-ped$Idmother[which(ped$animal%in%GP)]
              GGP<-unique(c(gp1,gp2))
              GGP<-GGP[!is.na(GGP)]#removing NA
              up<-c(P, GP,GGP)
              down1<-ped$animal[which(ped$Idfather%in%up)] #every child of P, GP and GGP
              down2<-ped$animal[which(ped$Idmother%in%up)]
              d1<-unique(c(down1,down2))
              d1<-setdiff(d1,spouse_ped)#removing spouse
              d1<-setdiff(d1,IDego)#removing ego
              down1<-ped$animal[which(ped$Idfather%in%d1)]  #one generation further down
              down2<-ped$animal[which(ped$Idmother%in%d1)]
              d2<-unique(c(down1,down2))
              d2<-setdiff(d2,spouse_ped)#removing spouse
              d2<-setdiff(d2,IDego)#removing ego
              u1<-ped$Idfather[which(ped$animal%in%d2)]# to catch the spouse if there are children
              u2<-ped$Idmother[which(ped$animal%in%d2)]#
              up2<-c(u1,u2)
              localSpousefamily<-unique(c(up,d1,d2,up2))  
             IDSpousefamily<-c(IDSpousefamily, localSpousefamily)  
            } 
            IDSpousefamily<-unique(c(IDSpousefamily,IDspouse))
            #Due to complex pedigrees, some former catagory could appear in the spouse family
            #Thus removing mock individuals, and the previous categories
            IDSpousefamily<-setdiff( IDSpousefamily,mock_individuals)
            IDSpousefamily<-setdiff( IDSpousefamily,IDEgoSibs)
            IDSpousefamily<-setdiff( IDSpousefamily,list_child)
            IDSpousefamily<-setdiff( IDSpousefamily,IDEgoGP)
            IDSpousefamily<-setdiff( IDSpousefamily,mother_father)
            IDSpousefamily<-setdiff( IDSpousefamily,Allego$animal)
            spouse<-fl2[which(fl2$animal %in%IDSpousefamily),] 
################### Other categories           ####
        #grouping : ego, sibs, child, parents, grand-parents, spouse and spouse's family
            Allego$id  <-"Focal"
            sibs$id    <-"Sibs"
            child$id   <-"Childl"
            parents$id <-"Parents"
            GP1$id     <-"GP"
            spouse$id  <-"Spouse"
            member<- rbind(Allego,child, parents, GP1,sibs, spouse)
            # Removing duplicates
                pre<-member$animal[1]
                member$duplicate<-0
                for (i in 2:nrow(member)) {
                    if(member$animal[i]%in%pre){member$duplicate[i]<-1}
                     pre<-c(pre,member$animal[i])  
                }
                members<-member[which(member$duplicate==0),]
       # Other category
        other<-setdiff(fl2$animal,members$animal) 
        flother<-fl2[which(fl2$animal%in%other),]
        flother<-flother[-which(flother$animal%in%mock_individuals),]
        flother$id<-"Other"
        flother$duplicate<-0
        all<-rbind (members, flother)
        
################### Data in Table 1            ####        
        tt<-table(all$id,all$handedness)
              #          0    1
              # Childl  1393  125
              # Focal    598  102
              # GP       974   23
              # Other   3028  213
              # Parents 1251   85
              # Sibs    2663  167
              # Spouse   828   40
        margin.table(tt, margin =2)
              #        10735  755 
        margin.table(tt, margin =1)
        # Childl   Focal      GP   Other Parents    Sibs  Spouse 
        # 1518     700     997    3241    1336    2830     868  
        (tt[,2]/ margin.table(tt, margin =1))
        # Childl      Focal         GP      Other    Parents       Sibs     Spouse 
        # 0.08234519 0.14571429 0.02306921 0.06572046 0.06362275 0.05901060 0.04608295  