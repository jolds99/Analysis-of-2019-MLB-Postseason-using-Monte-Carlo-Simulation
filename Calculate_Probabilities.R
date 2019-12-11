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


# Calculating AL Advancement to LDS Probabilities
ALWC = read_csv("alwc.csv")
OAK_Adv_LDS = (sum(ALWC$Winner == "OAK")/100000)
TBR_Adv_LDS = (sum(ALWC$Winner == "TBR")/100000)

# Calculating AL Advancement to LCS Probabilities
ALDS1 = read_csv("alds1.csv")
ALDS2 = read_csv("alds2.csv")
ALDS3 = read_csv("alds3.csv")
MIN_Adv_LCS = (sum(ALDS1$`Series Winner` == "MIN")/100000)
NYY_Adv_LCS = (sum(ALDS1$`Series Winner` == "NYY")/100000)
OAK_Adv_LCS = OAK_Adv_LDS * (sum(ALDS2$`Series Winner` == "OAK")/100000)
TBR_Adv_LCS = TBR_Adv_LDS * (sum(ALDS3$`Series Winner` == "TBR")/100000)
HOU_Adv_LCS = (OAK_Adv_LDS*(sum(ALDS2$`Series Winner` == "HOU")/100000)) + 
  (TBR_Adv_LDS*(sum(ALDS3$`Series Winner` == "HOU")/100000))

# Calculating AL Advancement to WS Probabilities
ALCS1 = read_csv("alcs1.csv")
ALCS2 = read_csv("alcs2.csv")
ALCS3 = read_csv("alcs3.csv")
ALCS4 = read_csv("alcs4.csv")
ALCS5 = read_csv("alcs5.csv")
ALCS6 = read_csv("alcs6.csv")
MIN_Adv_WS = (MIN_Adv_LCS * 
                (((sum(ALCS1$`Series Winner` == "MIN")/100000)*(HOU_Adv_LCS)) + 
                   ((sum(ALCS5$`Series Winner` == "MIN")/100000)*(OAK_Adv_LCS)) + 
                   ((sum(ALCS6$`Series Winner` == "MIN")/100000)*(TBR_Adv_LCS))))
NYY_Adv_WS = (NYY_Adv_LCS * 
                 (((sum(ALCS2$`Series Winner` == "NYY")/100000)*(HOU_Adv_LCS)) + 
                    ((sum(ALCS3$`Series Winner` == "NYY")/100000)*(TBR_Adv_LCS)) + 
                    ((sum(ALCS4$`Series Winner` == "NYY")/100000)*(OAK_Adv_LCS))))
HOU_Adv_WS = (HOU_Adv_LCS * 
                (((sum(ALCS1$`Series Winner` == "HOU")/100000)*(MIN_Adv_LCS)) + 
                   ((sum(ALCS2$`Series Winner` == "HOU")/100000)*(NYY_Adv_LCS))))

OAK_Adv_WS = (OAK_Adv_LCS * 
                (((sum(ALCS4$`Series Winner` == "OAK")/100000)*(NYY_Adv_LCS)) + 
                   ((sum(ALCS5$`Series Winner`== "OAK")/100000)*(MIN_Adv_LCS))))
TBR_Adv_WS = (TBR_Adv_LCS * 
                (((sum(ALCS3$`Series Winner` == "TBR")/100000)*(NYY_Adv_LCS)) + 
                   ((sum(ALCS6$`Series Winner` == "TBR")/100000)*(MIN_Adv_LCS))))
