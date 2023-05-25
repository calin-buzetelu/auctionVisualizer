# AuctionVisualizer

## Important
There is only one commit inside this project as this project has been fully implemented during 2021 as part of my Bachelor's thesis at the Chair of Decision Sciences & Systems at the Technical University of Munich. Initially, the code was published to a GitLab repository, to which I no longer have access.

The purpose of this document is to offer a brief summary of the code in the repository, as well as the correct setup and usage of the tool.

### Application set-up:

Once in your local environment, you can open the project in RStudio. If the used libraries aren't installed in your local machine, you have to manually install them before the usage of the app.
To do so, you can type in the console of RStudio the following: 

    install.packages("_**name_of_library**_")

The libraries can be found in the "Libraries" section of the app.R file

## Code structure:

The whole logic of the web application lies in the app.R file. Once opened, the structured comments will guide the user towards the area of interest.
In their order, they are:
1. Libraries
2. Auxiliary variables
3. User interface
4. Application logic
    
    4.1. About section
    
    4.2. Auction simulations
    
    4.3. Auction logic of simulation
    
    4.4. Graph generations
    
    4.5. Auxiliary functions
    
    4.6. UI and logic for user-defined bidding function
    
    4.7. Draw valuations
    
    4.8. Bidding Strategies
    
    4.9. Payment allocation mechanism
    
    4.10. Auction mechanism
### Starting the application:

Once the previously mentioned steps have been executed, you will now be able to start the tool. In the upper right-hand side of RStudio, you should either see a button entitled "Run App", or "Run". If the first one appears, just click on it, and RStudio will provide you with a second window where the tool will start. In case the latter one appears, select the whole code, and press the button. The behavior should be just like the one in the first case.
