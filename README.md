# project_3
This is a project for the STAT 400 course during the Fall 2019 semester at Colorado State University.
The members of this group are students Quinn Johnson, Jonathan Olds and Andrew Wiggans.
Our project's goal is to use Monte Carlo simulation to determine if the the 'best' team in the 2019 MLB Postseason won the World Series (based on a 2012 article by Thomas W. Rudelius).

There are 8 R scripts in the repository. The order in which they should be run to ensure complete and successful reproducibility is:
  - Fix_Pitching.R
  - Batting_Probabilities.R
  - Defense.R
  - Run_Inning.R (this contains many simulations that are very time consuming and are not necessary to run the rest of the files. Our results from the simulations are in the Inning_Sims.csv)
  - Run_Game.R
  - Run_Series.R (this also contains many simulations that took the three of us about 5-6 days to run. We would not advise running the series simulations. Our completed simulations are in the many csv's in the repository that read like ""wc.csv, ""ds".csv, ""cs".csv and ws".csv)
  - Calculate_Probabilities.R
  - Create Charts & Graphs.R
  
  
Be sure to load the images.zip file to load the pictures for the paper and presentation. We decided to use pictures in the markdown rather than the actual code because the outcome will be the same (this can be checked by running the various charts and graphs), but in a more simple and guaranteed way.
  
The final paper is "finalpaper.pdf" and the final presentation is "final_presentation_pdf.pdf".
