# A Globalization Dashboard
## Final project for Introduction to Data Science by Federico Mammana, Francesco Danovi & Milan Schröder (Hertie School)

We aim to fill a gap in research resources: 
Currently, there is only one curated Index of Globality (i.e., the level of Globalization) available to researchers interested in a measurement of globalization as a multidimensional phenomenon beyond its pure economic dimension – the KOF Globalisation Index (Dreher et al. 2008; Gygli et al. 2018). This index, however, applies a “flexible definition” (Gygli et al. 2018), operationalizing globalization as a catch-all-term that incorporates theoretically distinct concepts (e.g., cross-border interaction, integration, interdependence) in a complex but rather arbitrary fashion (see Kessler 2016; Schröder 2020). Indicators of these concepts yield empirically strongly differential results. The KOF’s dominance seems to originate less from its validity than its availability.


To provide an alternative to scholars interested in a more valid measure, we plan to construct an updated version of the Kessler Globality Index (Kessler 2016), that was only constructed for a single year yet. The KGI follows the contrary approach to the KOF, focussing on a clear definition, the strong validity of its indicators, and simple replicability. This makes it feasible to construct the KGI for a wide range of countries over a period of at least up to 30 years, using data available via APIs mostly. With approval of the author, we will include some minor updates and present the data as an interactive dashboard. We will proceed by illustrating what we mean by globality measures and globalization. Later on, we will discuss the KOF index, what parameters it follows and why we identify the need to offer an alternative index. We will then delve into KGI, by explaining how it its structure and benefits. Finally, we will illustrate our dashboard in detail and how it can complement existing research resources.

## Globalization measurement
Although it is hard to find a precise definition of globalization everyone can agree on, we believe that globality measures should:

a) depict the cross-border interaction of individuals;

b) be clearly delineated from ambiguous terms;

c)include the socio-cultural, political and economic dimensions (without weighting them arbitrarily);

d) be spatially and temporally adequate;

e) show clear distinction from causes and consequences.

When measured, the process of globalization must go beyond a single point in time and be chronologically repeatable.
In the following, existing approaches for measuring globalization will first be examined in detail with regard to these criteria. 

In line with the conceptual disagreement, the literature contains a large number of indicators for measuring globalization. By means of a comparison with the KGI, Kessler (2016: 172-175) developed some alternative measures, showing that even a single valid indicator can be superior to complex multiple measures. As he himself notes, the tested indicator, the per capita standardized foreign trade volume, is itself part of the KGI and has by far the best data availability of all the indicators included (ibid .: 165). Multiple globality measurements have - in addition to the mapping of the multi-dimensionality of the phenomenon - in principle, however, the advantage of being more robust against random measurement errors, outliers and the influence of insufficiently valid indicators, as these are affected by other indicators and tend to be evened out. However, all individual indicators should represent the common theoretical construct with satisfactory validity and correlate strongly with one another in the sense of the criterion of convergence validity. If this criterion is not met, it could be argued that the theoretical construct consists of mutually independent dimensions, which however, it is not convincing because the measurement would then lead to arbitrarily different results depending on the choice of dimensions and indicators (ibid .: 149-156).

It should be noted, however, that Kessler (2016: 149-160) found far-reaching deficiencies in the validity of various globalization indexes. This especially applies to the KOF index, which has since been partially revised, the results of which are published annually, and which is probably also the most common multiple measure for globalization for this reason. The KOF index is therefore to be examined in the following and compared to the KGI, which is particularly concerned with validity.

### The problems with existing measures: The KOF Globalization Index
The KOF Globalization Index is an index that provides annual data on the level of globalization at the level of nation states. Globalization is defined here as the “process of creating networks of connections among actors at multi-continental distances, mediated through a variety of flows including people, information and ideas, capital and goods” (Gygli et al. 2019: 546). The concept here corresponds to a catch-all approach that combines the understanding of interaction, integration and interdependence. Aspects of globalization as universalization, such as “the global spread of ideas” (ibid.), are part of the globalization concept used as a core component of the social dimension. In particular, the sub-dimension of cultural proximity also includes the understanding of globalization as “westernization” in the concept, because according to the authors, cultural globalization denotes the “domination of U.S. cultural products "(ibid.). Already the theoretical conception of the KOF-Index thus shows a contradiction to the demarcation of cross-border interaction from alternative globalization terms required in the above analysis framework.
With the current edition, the KOF-Index has undergone a far-reaching revision: while 23 indicators were included in the 2007 KOF-Index, after the revision there were almost twice as many. 

A fundamental problem of the KOF is based on its very broad concept of globalization. Following this catch-all term, some indicators do not depict cross-border interaction, but one or more of the globalization concepts. In addition to the number of McDonald's restaurants and IKEA stores that depict globalization (at most) as Americanization or Westernization (Kessler 2016: 157), the many indicators that depict globalization as integration, i.e. cross-border interaction, are particularly noticeable in relation to internal interaction. This applies to almost all indicators of economic de facto globalization. The standardization of the indicators with GDP can be seen as a clear indication of this. The number of memberships in intergovernmental organizations should also be seen as an indicator of political integration. 

A large number of indicators of all dimensions also violate the requirement set out above for an adequate representation of the phenomenon in terms of time: all those variables that depict stocks instead of the more volatile flows are obviously problematic for measuring the annual globalization level. The problem exists, for example, with trade agreements and the proportion of the population born abroad, which is included in the KOF as a migration indicator (cf. ibid .: 153). 

Some indicators, such as the number of embassies or NGOs, are also included in the index as absolute values ​​in a completely unstandardized manner: if, however, there are as many embassies in a country the size of China as in a small state, the latter would, according to the general understanding, be much more globalized. Standardization with the population size, which is missing here, is therefore recommended (cf. ibid .: 151-153). 

The most dramatic problem, however, is the lack of separation between the globalization process and its (possible) causes and consequences: this affects almost the entire de jure globalization index. While in the original version of the KOF only the economic dimension was subordinated to a sub dimension “Restrictions” with a weight of 50% (Dreher et al. 2008: 47f.), enabling factors in the latest edition have a share of 50% in the overall index (Gygli et al. 2019: 545). For many of the variables included, it also appears to be questionable whether they can even be regarded as enabling factors or consequences. Whether, for example, telephone and television connections actually lead to cross-border interaction can certainly be doubted. It is much more likely that most of the usage will be limited to national TV channels and that domestic calls will be predominantly made. 
Formal education, civil liberties and freedom of the press may at least be related to globalization as enabling factors or consequences, but the inclusion of such indicators in the KOF index implies an implicit assessment of globalization as a just, democratic development that has yet to be proven. The investigation of such relationships would be a typical application of a globalization measure, for which the KOF disqualifies itself by including central normatively relevant dependent variables in the independent variable. While the above-mentioned variables can still be justified to some extent comprehensibly, at least in terms of understanding globalization as universalization (Gygli et al. 2019: 557), gender parity as an indicator of globalization can still be surprising. One could argue that the universalization of a “western” value of gender equality could be mapped, but global dissemination requires a starting point. To see such a “place of origin” of gender equality in the “western” world seems at least somewhat debatable, if not presumptuous.

In summary, the validity of the content of many of the KOF indicators must be seriously questioned for various reasons - in particular the mixture of globalization with enabling factors and integration. 

(Possible tables?)
The weighting procedure is probably the heart of the KOF. However, the weighting on the upper levels is arbitrary. De facto and de jure globalization make up 50% each, which in turn are made up of 33.3% each from the three defined dimensions. The economic dimension in turn consists of two sub-indices, the social of three, the political, on the other hand, has no sub-indices whatsoever (Gygli et al .: 545), which means that the total weight of the indicators depends not only on the calculated weights, but on the (somewhat arbitrary) classification under dimensions and sub-indices.
In addition to the inadequate theoretical foundation and the content-related validity problems of the individual indicators, the ultimately arbitrary weighting speaks against using the KOF index as a measure of globalization without hesitation.

### Alternative measure: The Kessler Globality Index (KGI)
The Kessler Globality Index (KGI) is currently only available for a single year of investigation, 2016, which is obviously problematic for the investigation here, since only the globality of the subjects of investigation, instead of possible globalization processes, could actually be investigated.

The KGI pursues a fundamentally different approach than the KOF Index: It is "more important that each individual indicator represents the selected theoretical construct in a satisfactorily valid manner than on maximizing the number of indicators" (Kessler 2016: 164) . The KGI also has significantly higher demands on the theoretical justification than the KOF. Attention is paid to an adequate measurement of globality in the sense of the concept of cross-border interaction, to a spatially and temporally appropriate recording, and it avoids mixing with related concepts.
The simplicity can be seen in its calculation: the index is composed of seven indicators that are standardized, consistently averaged per capita and equally weighted, as usual, for a uniform value range (ibid .: 160),

This already has the advantage over the KOF of a less arbitrary weighting. Equally weighting the individual indicators would be problematic if several of the variables represented the same aspect of globalization and this would therefore be implicitly weighted twice. However, this only seems conceivable in one place: the definition of the tourism indicator of the World Bank includes not only leisure tourism trips, but is defined as the “activity of people traveling to and staying in places outside their usual environment for no more than one year for leisure, business, and other purposes not related to an activity remunerated from within the place visited”(World Bank 2020), thus includes almost all non-permanent entries and exits, including business trips. However, this results in a strong overlap with the air traffic indicator used in the KGI. Obviously planes are not the only form of travel, but in many cases this accounts for the majority of trips, which is why an extremely strong correlation between the two indicators is to be expected. The strong correlation between all indicators that the KGI assumes should ultimately ensure that such a weighting is empirically not too significant. For the above-mentioned reasons, however, it should be avoided.

The indicators used are the volume of foreign trade, as well as foreign direct investments (FDIs; sum of inflows and outflows; both in US dollars), international meetings , international arrivals and departures at commercial airports, international tourist arrivals and departures, international incoming and outgoing telephone traffic in minutes and the estimated number of people with Internet access, each per capita (ibid .: 161-164). 
Correlation and factor analysis were used to examine whether the indicators actually depict the same theoretical construct. The analysis consistently showed very strong relationships between the individual indicators, all of which also have a very strong impact on a single factor (ibid .: 164-167). Attention is paid to the occurrence of all dimensions in the KGI - even if the assignment is only a tendency.
There are only more serious validity concerns for one single indicator, Internet users. On the one hand, this is not about the interaction process itself, but rather an enabling factor which cannot distinguish between domestic and cross-border interaction via the Internet. Since access to the Internet is also increasing globally, further use of this indicator would presumably lead to an approximation of usage figures to 100% in many countries in the future and the potentially serious differences in usage intensity between the countries would be completely ignored. Since complete coverage with Internet connections does not, of course, mean that globalization has reached its maximum, the fixed limitation of the scale must be viewed as problematic.

There are no fundamental problems with the other indicators, only minor optimization possibilities or concerns about the suitability of the data used. 
The total index is finally calculated using a simple additive procedure with the same weighting, that is, by calculating the arithmetic mean from all existing values. A KGI value is shown when at least three of the seven values ​​are available. Due to the strong correlations between the individual indicators, this appears to be entirely justifiable, since it can be assumed that a single value present represents a satisfactorily valid proxy for the level of globalization (Kessler 2016: 168f.). Since the KGI has proven to be a theoretically superior measure of globality, a slight revision and reproduction for the years 1992-2017 will be carried out below and its validity will be tested.

# Our revised Dashboard
Our Dashboard uses data from 1990 to 2020. As previously explained, its aim is to replicate the KGI for this range of years. The indicators used to assess the level of globalization are: population, area, internet users, level of imports, level of exports, foreign direct investments (in and out), level of tourism in and out. The following indicators were not available via WDI but can be made available upon request: International telephone traffic (ITU), International Meetings/Conferences (UIA) and International aircraft passengers (ICAO). 

For the construction of an informative index with an easily interpretable range, data was first per capita- and then panel-normalized. To identify the maximum and minimum values for normalization we excluded small states (as defined by Kessler 2016) and extreme outliers (outside 25% quantile - 3 * IQR, 75% quantile + 3 * IQR, see Schröder 2020). 
We then normalized the data on a scale ranging from 0 to 100.
Finally, we combined the variables that are all theoretically valid, load strongly on a common factor and are highly intercorrelated (Kessler 2016, Schröder 2020); for this reason, the index can be constructed simply by taking the average of all available normalized variables.

The following step was to build an interactive dashboard in the form of a map, where countries are given different colours according to the level of globalization reached. We cover all years from 1990 to 2020 (with varying data quality)
A small table stating the top 10 countries is provided as well.

Users get the option to select between Kessler's (2016) original KGI (as described above), and a refined edition developed by Schröder (2020). While the latter has some theoretical benefits due to newly available indicators that more precisely depict the volume and reach of cross-boarder interactions, the original index has advantages in terms of data coverage. By providing both alternatives, we allow interested researchers to test the robustness of their findings based on an alternative measure.

Additionally, users can choose a minimal number of indicators as a condition to be included in the output, animate the global changes over time, zoom in, find detailled information about the method, contributors, and download the datein several formats.


### References:
Dreher, Axel; Gaston, Noel; Martens, Pim (2008): Measuring Globalisation. New York, NY: Springer.
Gygli, Savina; Haelg, Florian; Potrafke, Niklas; Sturm, Jan-Egbert (2019): The KOF Globalisation Index – revisited. In: The Review of International Organizations 14 (3), S. 543– 574.

Kessler, Johannes (2016): Theorie und Empirie der Globalisierung. Wiesbaden: Springer Fachmedien.

Schröder, Milan (2020): Gerechte Globalisierung? Zur Messung von Globalisierungsprozessen und ihrem Einfluss auf die Verteilungsgerechtigkeit. Hausarbeit zur Erlangung des akademischen Grades Bachelor of Arts in Politikwissenschaft. Sozialwissenschaften, Medien und Sport der Johannes Gutenberg-Universität Mainz. Available online: https://hertieschool-my.sharepoint.com/:b:/g/personal/204856_hertie-school_org/EWp8tWUAvD5IjOmgbVoQjd8BaXL1bQNd_nuxH0u5ftVhoQ?e=DUxeYO 

#### Data Sources:
Central Intelligence Agency (CIA) (2018): The World Factbook 2016-17. Washington, DC.

International Civil Aviation Organisation (ICAO) (1993-2018): The World of Air Transport. Presentation of Air Transport Statistical Results. Compilation from the ICAO Annual Report to the Council.

International Civil Aviation Organisation (ICAO) (2019): State Traffic Statistics. Available online: https://www.icao.int/safety/iStars/Pages/API-Data-Service.aspx.

International Telecommunication Union (ITU) (2018): World Telecommunication Indicators Database.

Union of International Associations (UIA) (2001-2018): Yearbook of International Organizations. Guide to Global and Civil Society Networks. Vol 5: Statistics, Visualizations and Patterns. Leiden: Brill.

United Nations, Department of Economic and Social Affairs, Population Division (UN DESA) (2019): World Population Prospects 2019. Custom data acquired via website. Available online: https://population.un.org/wpp/DataQuery/.

World Bank (2020): World Development Indicators. Available online: https://databank.worldbank.org/source/world-development-indicators.
