# About
Jacob Townson & Andrew Giles  
July 21, 2015  

# Overview
This app works with the math behind an evolution and ecology course. It is a joint project between Jacob Townson and Andrew Giles of Georgetown College for a research project. The app is used to show/simulate a population's growth when there is a specific carrying capacity, birth rate, and death rate. The app's purpose is to help teach an evolution and ecology class. Below we will explain the purpose and uses of the nav bar, the tabs, as well as the meaning behind the variables input by the user in the sidebar.

# Sidebar

- **Time**: This lets you choose the time value displayed on the x axis of the graph under the *Population Size* tab.

- **Initial Population**: This lets you choose the initial population, or the y intercept under the *Population Size* tab.

- **Birth Rate**: This lets you choose the maximum birth rate for the population. The effective birth rate will change according to how close the population gets to the carrying capacity.

- **Death Rate**: This lets you choose the minimum death rate for the population. The effective death rate will change according to how close the population gets to the carrying capacity.

- **Carrying Capacity**: This lets you choose the carrying capacity for a population. It could be thought of as the amount of resources a population has to survive/thrive. Note, carrying capacity option is removed if death rate is above birth rate because a carrying capacity doesn't affect this given situation.

- **Setting the Seed**: This check box gives the user the option to set the seed of the simulated values. By setting the seed, the user can get the same outcomes for every time the *Simulate* button is pushed while the seed is set to that specific value. When the box is unchecked, the option goes away and the simulation is random again.

- **Simulate**: This button runs the app with a new simulation with the given input parameters.

# Population Graphs

This section is the main portion of the application. This is the only tab available before the **Simulate** button is hit. This tab displays the graphs used to convey information on the population growth. Before the **Simulate** button is hit, the graph displays only the theoretical results. If the *Size* button is chosen, then the population size graph will be shown. This graph shows the growth of a population over an arbitrary amount of time as it approaches it's carrying capacity. Here the population is conveyed by the red line and the carrying capacity is shown in blue. The next option under this tab is to plot the growth rate. Here the growth rate is plotted against either time or the population itself. This switching between what is plotted on the x axis can help to teach students how the growth rate changes, and makes it easier to understand through a look at it from both perspectives. Similar to this structure, the next option under this tab is to plot the per capita growth rate. Again, the user can choose what to put on the x axis, either time or population to give the user an easier way of understanding depending on their perspective.

The only change that is made under this tab by hitting the **Simulate** button is that a simulated population is displayed on the *Size* graph. This is conveyed by a black line. This simulated line is made using our model and with the help of the rpois() function in R. For more information on the model and how the simulation works, skip down to the **Model Description** Section of the Help File.

# Field

This portion of the application displays a model for the field, which is a hypothetical situation for the simulated data shown in the *Population Graphs* tab. Here there are two plots next to each other, with an animation bar above them. The two plots are used to show the total population at the time that is shown in the animation slider. When the play button is hit below the slider, the slider will move from time value to the next time value for every second or so. Whatever the simulated population is at that time, that is the total number of dots on the plots. The plot on the left shows the adults in the field foraging for food for their children. 

The plot on the right shows the newborn rabbits in what is cleverly called the "Warren". Here, the litters are grouped closely together using a plotting system we made involving keeping litters close together using the rnorm() function. In the field function, you may also notice the color of the graph changing over time. If there is a carrying capacity used in the simulation, then the color of the graph will change. The idea is that if the rabbits are far away from the carrying capacity, then the field will be more green. On the other hand, if the population is above, close to, or on the carrying capacity, they will naturally be running out of resources, making the field a more brown and barren color. This makes the app more interactive, and maybe fun for the students to learn the concepts, and actually get the feeling that they are seeing the changes in the environment. 

Below both graphs, a table will be displayed if any rabbits were born and added to the Warren. The table will show the numbr of litters at that time, and also show the average size of the litters born.

One final feature of this tab happens when the simulated population dies out. You as the user will have to find out what happens then for yourself. 

# Graveyard

This portion of the application is similar to the field in that it is a model used to present hypothetical information. It uses the simulated population to show the deaths at each time. It is uses a similar format to the field in that it uses an animation slider to show the graveyard over time. 

This tab also gives the rabbits a cause of death, again, to give the user a feeling of the whole situation being more real. The deaths are divided into three categories, death by lawnmower, death by foxes, and death by malnutrition. The death by lawnmowers is more likely to happen when the population is small, and the death by foxes and malnutrition's likelihood go up as the population grows. The app also has an info link to tell the user this explanation. There is also a table to show the population and the carrying capacity at the bottom, just to remind the user to give more of a reference on the amount of deaths at that certain time. 

One final feature of this tab happens when the simulated population dies out again. You as the user will have to find out what happens then for yourself. 

<center> <h1>Thanks For Using Our App!</h1> </center>



