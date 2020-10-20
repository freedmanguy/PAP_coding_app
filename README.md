# PAP coding app
Code for creating a shiny app for coding textual observations into Policy Agendas Project.
The demo app in its final version can be viewed at https://freedmanguy.shinyapps.io/papcodingdemo/.

## App Features for Coders
The app offers a visual interface for coders to review observations and input a relevant code with the following features:
1. Observations are displayed in a random order.
2. Value validation - the input field only accepts values included in the PAP codebook.
3. PAP codebook in table format - for easy scrolling and word-based searching. 
4. Reviewing observations already-coded by users.
5. Recoding the PAP code for a particular observations.

## App Features for Project Managers
For the project manager, the app guarantees coders only use valid codes and cannot alter any fields aside from inputting a relevant PAP code. In addition, the project manager has access to the coding progress and coded files in real time.

## Requirements
For optimal use, project managers should create a separate app for each coder.
Finally, the app requires creating an authorized dropbox token to access and update relevant files. Follow the instructions of the R package rdrop2 to create the token.
