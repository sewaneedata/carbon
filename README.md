# The Data Analysis of the Zanmi Kafe

This is the dashboard that we created in collaboration with the Zanmi Kafe project and the Sewanee DataLab. Zanmi Kafe's work offsets carbon emissions by planting trees on Haitian farms, which improves economic stability for the farmers. Our client needed a way to pay farmers and to efficiently calculate carbon sequestration using a vast amount of raw data. To solve this issue we developed this interactive dashboard that displays and analyzes the data.

### Main Panel

The main panel of the dashboard allows users to compare carbon sequestration, carbon payments, and trees planted across multiple years and households. The radio buttons reflect key information from three different graphs that they correspond to. The value boxes give totals for carbon sequestered in tons, how much is paid to the farmers, and the amount of trees on each farm, and the three graphs show these totals broken down by tree species and compare carbon sequestration on the same farm or overall. 

### Regressions Tab

The regressions section is a tab where each tree's height and diameter was log transformed and grouped by species to be plotted as regressions. Normally, tree height and diameter should be correlated with each other, but these may not always be correlated due to errors and fraud. This tab allows the user to look at raw data points and visualize discrepancies between years and households.

### Allometric Tab

The allometric comparison tab compares allometric equations used in various calculations to calculate the amount of carbon sequestered. The equations function in slightly different ways leading to drastically different results and show immense differences in the amount of carbon absorbed. By comparing the allometric equations based on species and year, the project partner determined which calculations most accurately measure the carbon absorption of each tree species, and these equations were used in the Main Panel to show our findings.

### Future Goals

Hopefully, this dashboard will continue to remain useful by receiving new data from future years, as well as serve as a model for other small-scale carbon markets. Next steps include adapting the project to allow for faster growth and more in-depth analysis of the data sets. It is Zanmi Kafe's goal to partner with the Haiti Photo Project to build maps with geospatial data points that observe the changes in economic development caused by growing carbon markets through the evaluation of the farmer's photos. Finally, it is a goal to one day automate this entire process in such a way that drone imaging could measure tree growth and size and automatically upload the recorded data into our dashboard for analysis.

### Now how do you run the code?

Well, first you want to run `prep_data.R` and then you want to run `app.R`. `prep_data.R` will read in all of the data from the excels on the google drive and will create your dat.csv file while `app.R` is what creates your app.

In `prep_data.R` we had to read in the google sheets using googlesheets4 because gsheet refuses to read any sheets other than the first sheet. The authorization issue of googlesheets4 has been avoided by using a function called googlesheets4::gs4deauth().

In the app it is important to be aware of the years. The Main Panel of the dashboard has options for 2019, 2021, and all years. The code was written so that only the trees and carbon absorbed from 2019 or 2021 will be displayed. The all years option shows all trees and all carbon absorbed. It was important to distinguish between 2019 and 2021 because farmers can only be paid once for each ton of carbon absorbed. Hence, 2021 only shows the carbon absorbed and paid for in that year. This was not done for the other tabs, such as Regressions and Allometric Comparison. In the Regressions tab, 2019 shows only the 2019 trees and the 2021 option shows only the 2021 trees. We felt no need to include an all years option since this would be an overwhelming amount of data. The Allometric Comparison tab however no longer follows what was done in the other tabs. The year 2021 accounts for **all** carbon absorbed by each tree in all of the years prior. This is something that could easily be changed if the Zanmi Kafe desired, however when the app was created there was no reason to distinguish between the years.

Good luck!

*Created in collaboration with Sewanee DataLab, Deborah McGrath, Nika Gorski, Kate Baker, and Caroline Willette*