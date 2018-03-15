# Creating Vote Margins for Mayoral Elections in Brazil (with Lucas Novaes and Leonardo Barone)

Repository with R code used to calculate vote margins in local elections in Brazil (2000-2016), using TSE Repositório data. Designs that use close mayoral races in Brazil are becoming increasingly common. However, using electoral data in Brazil requires the researcher to make several decisions. By making this code public, we hope to join the discussion on the use of electoral data in Brazil (see references below) and help to create a common set of standards to deal with the intricacies of this type of data.

Overall, our strategy was to analyze each election separately. Since the variables and quality of the data varies by election year, several of the issues we encountered also varied by year. This makes the code longer but we hope that our results are better by treating each election separately and then combining them in a standardized dataset. 

The main challenge was to separate "regular" from "nonregular" elections (those that take place off-cycle). Unfortunately, that is not always clear in the TSE data. Moreover, several candidates may drop out of the before, after, or during the campaigns and that impacts how researchers estimate candidates' final vote shares and vote margins. There are also cases in which candidates are not listed in the candidates' dataset but they are listed in the datasets that contain the electoral returns. Hopefully, our code contains all relevant comments to make our choices clear in dealing with each challenge.

For any questions of comments, please feel free to add an issue to this repository or write to us directly at natalia.bueno@emory.edu, lucas.novaes@iast.fr or leobarone@gmail.com.

Further references: 

For an excellent R package to download, analyze, and explore electoral data, see: https://github.com/silvadenisson/electionsBR

See CEPESP Data for an initiative to make the use and analysis of electoral data more accessible:http://cepesp.io/
