# Download data from 
# http://www.oecd.org/pisa/pisaproducts/pisa2012database-downloadabledata.htm
# Positions are in 
# http://www.oecd.org/pisa/pisaproducts/PISA12_stu_codebook.pdf

library(readr)

# PISA 2012
pisa2012 <- read_fwf(
  file="~/Downloads/INT_STU12_DEC03.txt",   
  fwf_positions(c(1, 897, 901, 1150+9*(0:4), 1510+9*(0:4), 1555+9*(0:4),1600,1609+9*(0:79),2342),
                c(3, 900, 904, 1158+9*(0:4), 1518+9*(0:4), 1563+9*(0:4),1608,1617+9*(0:79),2348),
                c("CNT", "OCO1", "OCO2", paste0("PV",1:5,"MATH"), paste0("PV",1:5,"READ"), paste0("PV",1:5,"SCIE"),"W_FSTUWT",paste0("W_FSTR",1:80),"VER_STU")),
  col_types = paste0("ccc",paste(rep("n",3*5 + 1 + 80), collapse=""),"c"))


