install.packages("tidyverse")
library(tidyverse)
# Calculating NL Advancement to LDS Probabilities
NLWC = read_csv("nlwc.csv")
WSN_Adv_LDS = sum(NLWC$Winner == "WSN")/100000
MIL_Adv_LDS = sum(NLWC$Winner == "MIL")/100000

# Calculating AL Advancement to LDS Probabilities
ALWC = read_csv("alwc.csv")
OAK_Adv_LDS = (sum(ALWC$Winner == "OAK")/100000)
TBR_Adv_LDS = (sum(ALWC$Winner == "TBR")/100000)

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

# Calculating NL Winning WS Probabilities
WS1 = read_csv("ws1.csv")
WS2 = read_csv("ws2.csv")
WS3 = read_csv("ws3.csv")
WS4 = read_csv("ws4.csv")
WS5 = read_csv("ws5.csv")
WS6 = read_csv("ws6.csv")
WS7 = read_csv("ws7.csv")
WS8 = read_csv("ws8.csv")
WS9 = read_csv("ws9.csv")
WS10 = read_csv("ws10.csv")
WS11 = read_csv("ws11.csv")
WS12 = read_csv("ws12.csv")
WS13 = read_csv("ws13.csv")
WS14 = read_csv("ws14.csv")
WS15 = read_csv("ws15.csv")
WS16 = read_csv("ws16.csv")
WS17 = read_csv("ws17.csv")
WS18 = read_csv("ws18.csv")
WS19 = read_csv("ws19.csv")
WS20 = read_csv("ws20.csv")
WS21 = read_csv("ws21.csv")
WS22 = read_csv("ws22.csv")
WS23 = read_csv("ws23.csv")
WS24 = read_csv("ws24.csv")
WS25 = read_csv("ws25.csv")


STL_Win_WS = (STL_Adv_WS * 
              (((sum(WS3$Champion == "STL")/100000)*(TBR_Adv_WS)) + 
                 ((sum(WS4$Champion == "STL")/100000)*(NYY_Adv_WS)) + 
                 ((sum(WS5$Champion == "STL")/100000)*(HOU_Adv_WS)) + 
                 ((sum(WS8$Champion == "STL")/100000)*(OAK_Adv_WS)) + 
                 ((sum(WS16$Champion == "STL")/100000)*(MIN_Adv_WS))))

ATL_Win_WS = (ATL_Adv_WS * 
                (((sum(WS9$Champion == "ATL")/100000)*(TBR_Adv_WS)) + 
                   ((sum(WS10$Champion == "ATL")/100000)*(OAK_Adv_WS)) + 
                   ((sum(WS17$Champion == "ATL")/100000)*(NYY_Adv_WS)) + 
                   ((sum(WS19$Champion == "ATL")/100000)*(MIN_Adv_WS)) + 
                   ((sum(WS20$Champion == "ATL")/100000)*(HOU_Adv_WS))))
                
LAD_Win_WS = (LAD_Adv_WS *
                (((sum(WS15$Champion == "LAD")/100000)*(TBR_Adv_WS)) + 
                   ((sum(WS18$Champion == "LAD")/100000)*(OAK_Adv_WS)) + 
                   ((sum(WS21$Champion == "LAD")/100000)*(MIN_Adv_WS)) + 
                   ((sum(WS23$Champion == "LAD")/100000)*(HOU_Adv_WS)) + 
                   ((sum(WS24$Champion == "LAD")/100000)*(NYY_Adv_WS))))

MIL_Win_WS = (MIL_Adv_WS * 
                (((sum(WS1$Champion == "MIL")/100000)*(NYY_Adv_WS)) + 
                ((sum(WS2$Champion == "MIL")/100000)*(HOU_Adv_WS)) + 
                ((sum(WS6$Champion == "MIL")/100000)*(TBR_Adv_WS)) + 
                ((sum(WS12$Champion == "MIL")/100000)*(OAK_Adv_WS)) + 
                ((sum(WS25$Champion == "MIL")/100000)*(MIN_Adv_WS))))

WSN_Win_WS = (WSN_Adv_WS * 
               (((sum(WS7$Champion == "WSN")/100000)*(TBR_Adv_WS)) + 
                ((sum(WS11$Champion == "WSN")/100000)*(NYY_Adv_WS)) + 
                ((sum(WS13$Champion == "WSN")/100000)*(OAK_Adv_WS)) + 
                ((sum(WS14$Champion == "WSN")/100000)*(MIN_Adv_WS)) + 
                ((sum(WS22$Champion == "WSN")/100000)*(HOU_Adv_WS))))

MIN_Win_WS = (MIN_Adv_WS * 
                (((sum(WS14$Champion == "MIN")/100000)*(WSN_Adv_WS)) + 
                   ((sum(WS16$Champion == "MIN")/100000)*(STL_Adv_WS)) + 
                   ((sum(WS19$Champion == "MIN")/100000)*(ATL_Adv_WS)) + 
                   ((sum(WS21$Champion == "MIN")/100000)*(LAD_Adv_WS)) + 
                   ((sum(WS25$Champion == "MIN")/100000)*(MIL_Adv_WS))))

NYY_Win_WS = (NYY_Adv_WS * 
                (((sum(WS1$Champion == "NYY")/100000)*(MIL_Adv_WS)) + 
                   ((sum(WS4$Champion == "NYY")/100000)*(STL_Adv_WS)) + 
                   ((sum(WS11$Champion == "NYY")/100000)*(WSN_Adv_WS)) + 
                   ((sum(WS17$Champion == "NYY")/100000)*(ATL_Adv_WS)) + 
                   ((sum(WS24$Champion == "NYY")/100000)*(LAD_Adv_WS))))

HOU_Win_WS = (HOU_Adv_WS * 
                (((sum(WS2$Champion == "HOU")/100000)*(MIL_Adv_WS)) + 
                   ((sum(WS5$Champion == "HOU")/100000)*(STL_Adv_WS)) + 
                   ((sum(WS20$Champion == "HOU")/100000)*(ATL_Adv_WS)) + 
                   ((sum(WS22$Champion == "HOU")/100000)*(WSN_Adv_WS)) + 
                   ((sum(WS23$Champion == "HOU")/100000)*(LAD_Adv_WS))))

TBR_Win_WS = (TBR_Adv_WS * 
                (((sum(WS3$Champion == "TBR")/100000)*(STL_Adv_WS)) + 
                   ((sum(WS6$Champion == "TBR")/100000)*(MIL_Adv_WS)) + 
                   ((sum(WS7$Champion == "TBR")/100000)*(WSN_Adv_WS)) + 
                   ((sum(WS9$Champion == "TBR")/100000)*(ATL_Adv_WS)) + 
                   ((sum(WS15$Champion == "TBR")/100000)*(LAD_Adv_WS))))

OAK_Win_WS = (OAK_Adv_WS * 
                 (((sum(WS8$Champion == "OAK")/100000)*(STL_Adv_WS)) + 
                 ((sum(WS10$Champion == "OAK")/100000)*(ATL_Adv_WS)) + 
                 ((sum(WS12$Champion == "OAK")/100000)*(MIL_Adv_WS)) + 
                 ((sum(WS13$Champion == "OAK")/100000)*(WSN_Adv_WS)) + 
                 ((sum(WS18$Champion == "OAK")/100000)*(LAD_Adv_WS))))
