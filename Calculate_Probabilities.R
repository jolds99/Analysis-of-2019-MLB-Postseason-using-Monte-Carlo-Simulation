library(tidyverse)
# Calculating NL Advancement to LDS Probabilities
NLWC = read_csv("nlwc.csv")
WSN_Adv_LDS = sum(NLWC$Winner == "WSN")/100000
MIL_Adv_LDS = sum(NLWC$Winner == "MIL")/100000

# Calculating NL Advancement to LCS Probabilities
NLDS1 = read_csv("nlds1.csv")
NLDS2 = read_csv("nlds2.csv")
NLDS3 = read_csv("nlds3.csv")
STL_Adv_LCS = sum(NLDS1$`Series Winner` == "STL")/100000
ATL_Adv_LCS = sum(NLDS1$`Series Winner` == "ATL")/100000
MIL_Adv_LCS = MIL_Adv_LDS*(sum(NLDS2$`Series Winner` == "MIL")/100000)
WSN_Adv_LCS = WSN_Adv_LDS*(sum(NLDS3$`Series Winner` == "WSN")/100000)
LAD_Adv_LCS = (MIL_Adv_LDS*(sum(NLDS2$`Series Winner` == "LAD")/100000)) + 
              (WSN_Adv_LDS*(sum(NLDS3$`Series Winner` == "LAD")/100000))

#Calculating NL Advancement to WS Probabilities
NLCS1 = read_csv("nlcs1.csv")
NLCS2 = read_csv("nlcs2.csv")
NLCS3 = read_csv("nlcs3.csv")
NLCS4 = read_csv("nlcs4.csv")
NLCS5 = read_csv("nlcs5.csv")
NLCS6 = read_csv("nlcs6.csv")
STL_Adv_WS = (STL_Adv_LCS * 
             (((sum(NLCS1$`Series Winner` == "STL")/100000)*(LAD_Adv_LCS)) + 
              ((sum(NLCS5$`Series Winner` == "STL")/100000)*(MIL_Adv_LCS)) + 
              ((sum(NLCS6$`Series Winner` == "STL")/100000)*(WSN_Adv_LCS))))
ATL_Adv_WS = (ATL_Adv_LCS * 
                (((sum(NLCS2$`Series Winner` == "ATL")/100000)*(LAD_Adv_LCS)) + 
                   ((sum(NLCS3$`Series Winner` == "ATL")/100000)*(WSN_Adv_LCS)) + 
                   ((sum(NLCS4$`Series Winner` == "ATL")/100000)*(MIL_Adv_LCS))))
LAD_Adv_WS = (LAD_Adv_LCS * 
                (((sum(NLCS1$`Series Winner` == "LAD")/100000)*(STL_Adv_LCS)) + 
                ((sum(NLCS2$`Series Winner` == "LAD")/100000)*(ATL_Adv_LCS))))
MIL_Adv_WS = (MIL_Adv_LCS * 
              (((sum(NLCS4$`Series Winner` == "MIL")/100000)*(ATL_Adv_LCS)) + 
                 ((sum(NLCS5$`Series Winner`== "MIL")/100000)*(STL_Adv_LCS))))              
WSN_Adv_WS = (WSN_Adv_LCS * 
                (((sum(NLCS3$`Series Winner` == "WSN")/100000)*(ATL_Adv_LCS)) + 
                   ((sum(NLCS6$`Series Winner` == "WSN")/100000)*(STL_Adv_LCS))))              
