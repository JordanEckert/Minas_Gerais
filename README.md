# Minas_Gerais
Classification and exploratory analysis of heavy metal contaminants in Minas Gerais' soil. This is joint work with Nedret and JJ.

<b>Sampling design:</b>
The database comprises contaminated and uncontaminated soils, but for this study, I think that we should restrict uncontaminated samples (714 samples). Samples were collected to represent geological and pedological diversity. Sampling was avoided close to potential human disturbances such as roads, mining sites, and pastures. Each sample represents five individual samples collected and proportionally mixed in the lab. All analyses were performed in the composed sample. To avoid heavy metal contamination, we collected with inox tools.

<b>Quality Reference Values:</b>
The government supported this study to obtain data about heavy metal content in soils. These results would be used to create quality reference values (QRV) and identify contaminated sites. We suggested that Minas Gerais should create different (QRV) for each region according to geological diversity. Unfortunately, they did not listen to our suggestions. After excluding outliers, they create a unique QRV for all the State according to the 90th quartile.

<b>Questions:</b>

•	Could the samples be grouped according to heavy metal contents? If yes, are these groups spatially correlated?

•	Could we predict the heavy metal contents by the soil properties?

•	Are the heavy metal contents in rock related to the heavy metal contents in soil?

----

Tips on EDA from Billor: 

``I am sure you will be thinking about these, but I just wanted to provide  afew suggestions anyway…

Check the spatial correlation for each heavy metals variable (variagrom ?).

Check the correlation between the heavy metals' correlation (Correlation Matrix).

Check the correlation between soil properties  and Heavy Metal Cont levels (20 variables).

.. A good EDA is needed for this data..

Many PQLs are in the data table..

Anyway, hope that you enjoy this project. It is a good opportunity to learn new methodology. ""

----
Additional Data Cleaning:

- There seems to be both missing (NA) and (<PQL) values. Are they to be treated the same way?