# Sports-Injury-Analysis
This project analyzes sports injuries based on various attributes (products, body parts, gender, race, age, and severity). Using comprehensive data cleaning, aggregation, and visualization, it identifies trends, proportions, and insights to help improve safety and awareness.

## Tools & Techniques
R Studio

Libraries:
- dplyr for data manipulation.
- ggplot2 for visualization.
- tidyr for reshaping data.
- lubridate for date handling.
- stringr for string operations.
- treemapify for treemap visuals

## Dataset
A 2023 dataset consisiting of 306240 records. Data source: [National Electronic Injury Surveillance System (NEISS)](https://www.cpsc.gov/cgibin/NEISSQuery/home.aspx)

The cleaned dataset consists of 3192 records.

## Methods
### Key Analyses
- By Product: Identified top products with the highest injury counts and highlighted the most risky products based on the proportion of severe injuries.
- By Body Part: Determined the most commonly injured body parts and diagnoses associated with each product.
- By Gender: Analyzed injury proportions by gender across products, body parts, severity levels, and multi-body-part injuries.
- By Race: Explored injury distributions across races for products and body parts, identifying key racial trends.
- By Age: Segmented injuries by age groups and analyzed participation-specific trends.
- Severity: Categorized injuries into minor, moderate, and severe levels, with additional focus on alcohol and drug influences.

### Visualization
- Bar Charts: For product, gender, and race-specific trends.
- Treemaps: To visualize most common injuries by products and body parts.
- Pie Charts: For multi-body-part injuries and severity influenced by alcohol/drugs.
- Stacked Bar Charts: For age, race, and severity-based distributions.

## Results
This project demonstrates the power of data-driven insights in identifying injury patterns and promoting safer practices across sports and recreational activities. Some of the applications of this project could be:

- Identify products and activities requiring better safety measures.
- Develop target measures for injury prevention.
- Create awareness among people
