# About
Jacob Townson & Andrew Giles  
July 21, 2015  

# Overview
This app works with the math behind an evolution and ecology course. It is a joint project between Jacob Townson and Andrew Giles of Georgetown College for a research project. The app is used to show a population growth rate when there is a specific carrying capacity, birth rate, and death rate. Currently, the app is not finished for it's actual purpose, which is to help teach the evolution and ecology class, however, the main graph used to explain the growth is given under the *Population Size* tab. Below we will explain the purpose and uses of the rest of the tabs, as well as the meaning behind the variables input by the user in the sidebar. To see our code, and more details on how we did our work, feel free to visit our github repository for this project at <a href = "https://github.com/agiles231/shinyBio" target = "_blank"> this link. </a>

# Sidebar

- **Choose Plot**: This chooses whether you want to display the theoretical graph, the simulated data, or both under the *Population Size* tab.

- **Extent of Time**: This let's you choose the time value displayed on the x axis of the graph under the *Population Size* tab.

- **Initial Population**: This let's you choose the initial population, or the y intercept under the *Population Size* tab.

- **Max Birth Rate**: This let's you choose the maximum birth rate for the population. It will change according to how close the population gets to the carrying capacity.

- **Min Death Rate**: This let's you choose the minimum death rate for the population. It will change according to how close the population gets to the carrying capacity.

- **Carrying Capacity**: This let's you choose the carrying capacity for a population. It could be thought of as the amount of resources a population has to survive/thrive. Note, carrying capacity option is removed if death rate is above birth rate because a carrying capacity doesn't affect this given situation.

# Population Size

This section is the main portion of the application. This tab displays the chosen graphs from the sidebar and the carrying capacity line (if applicable) on a single graph. This graph can be used to explain to students how the population grows theoretically, and the simulated line shows how the situation may actually pan out in reality. If the maximum birth rate is higher than the minimum death rate, both the chosen graphs and the carrying capacity will be displayed. If the minimum death rate is higher than the maximum birth rate, the carrying capacity will not be displayed. In order to run the simulation, and print the new graph with the given inputs from the sidebar, the user must press the simulate button at the bottom of the sidebar.

## Notes on the Simulation

The simulation done in the *Population Size* tab is represented throughout the application. The simulation is run using a model our team created using the rpois() function to make Poisson random variables. More details on the model will be posted soon.

# Growth Rate

This portion of the app is not currently finished. More details will come on it later.

# Field

This portion of the application displays a model for the field, a hypothetical for the simulated data shown in the *Population Size* tab. The idea is that this graph presents a field that rabbits (the population) live in, and if you hit the play button or choose a time on the slider within the tab, you can watch the population grow and shrink within the range of the input of the *Extent of Time* variable. 

# Graveyard

This portion of the application is similar to the field in that it is a model used to present hypothetical information. It works in parallel to the field model, but instead of growing and shrinking, it just grows with the death from the simulation. So for every "rabbit" that dies in the population, a gravestone is added to the graveyard.

<center> <h1>Thanks For Using Our App!</h1> </center>



