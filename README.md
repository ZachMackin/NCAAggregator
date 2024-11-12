The intended use of my package is to aggregate predictions for college basketball. The main workhorse of the package is an aggregating function that takes in first a vector of either predictions of the form (home score, away score) or models that take in the college basketball data of the desired form and output a vector of that form. The package includes all of the aggregation functions that can be utilized by the model, and some example modeling functions to illustrate to the user how their functions should work to fit the package, and give them some existing functions with proven success. 

Installation Methodology: Run the command devtools::install_github("ZachMackin/NCAAggregator")

My work left is pretty well exemplified in the "[ToDos]" throughout the code and I will need to construct some example model functions, complete aggregation functions, add some checks to my main function, and implement some further testing. Additionally, I will construct a Vignette to show the methodology and show this methodologies success on unseen examples.  
