# MiLB Hitter Archetype Explorer

## Overview
This project is designed as a tool for player comparison and scouting high school, college, independent, and other baseball players.
When users enter hitter's statistics, they will receive an archetype assignment based on offensive characteristics, such as plate approach,
power, contact, baserunning, and batted-ball tendencies. A separate feature is a historical player search that allows users to select qualified 
hitters from the 2022 - 2025 Triple-A, Double-A, High-A, or Low-A seasons and identify their most closest player comparisons.

## Methodology
This project uses K-means clustering to group Minor League hitters into four offensive archetypes.

The clustering model uses six features:
- Pitched per plate appearance
- Strikeout rate
- ISO
- BABIP
- Stolen base attempt rate
- ground-ball to air-ball ratio

These variables were transformed when appropriate and standardized before clustering.

New players are assigned to the nearest cluster center using the same steps applied to the original training data. The similar historical players
are identified using Euclidean distance across the standardized model features.

## Data
The stated data was collected from MLB's publicly accessible Minor League statistics.

## Data Limitations
One limitations of the project is that not all model inputs may not be available for a player who is being evaluated. This would
limit the ability to generate a complete comparison. This analysis is also based on a select sample of players, rather than an entire population.
Because of this, the archetypes should be treated as descriptive and not definitive classifications of future performance. Lastly, the similarity 
distance is a comparative measure, not a probability.

## Future Improvements
Future improvements will include making advanced statistic calculations in the backend to enable users to enter readily available model inputs. This may require the clustering model that is used in the application to be changed or expanded.
