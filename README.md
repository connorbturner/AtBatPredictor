# AtBatPredictor

The attached R script, transition_matrix_maker.r, scrapes Statcast data for a specific pitcher and hitter over a certain timeframe and uses it to calculate 
the probabilities of a hit, walk, strikeout, or an out on a ball in an at-bat between the two. It does so by converting the data into a transition matrix for a 
Markov chain that simulates this at-bat. This script also includes a basic ggplot that displays the outcome probabilities for a certain count together in 
a bar graph. To learn more about how this project works, you can check out the full paper explaining it at the url below: 

# https://readthediamond.com/research/the-pinch-hitter-problem 
