# Newtwork_Analysis

Network Analysis Corp (NAC) was tasked with compiling company-level network data on the employees at Real Shade Sunglasses company (RSS). Members of the NAC data science team interviewed employees from each department of RSS: Executive, Marketing, Sales, HR, Distribution, Manufacturing, and Finance. The employees that were interviewed were asked basic questions about everyday life at their company in an effort to compile three different networks. The networks that were developed were based on the three research questions:

- Which coworkers do you notice spending time together during lunch break or outside of work?
- Who do you observe your coworkers interacting with each other on work-related matters?
- Who do you observe your coworkers giving advice to?


From those three questions we were able to generate three networks: Friendship, Work, and Advice. The Friendship Network houses information about employees who talked about hobbies, discussed their personal lives, and had informal discussions about work-related topics in a non-professional capacity. The Work Network housed all of the data on who was described working together on projects, giving updates on material, and engaged in meetings. The Advice Network has information on who gives professional advice to each other and can give insights into the hierarchy of the company and who feels like they need information and who gives it. The Advice Network also includes data from a mentorship program where employees were paired with each other based on seniority and a pre-filled personal survey in order to promote cross-department cohesion within the company. This data was added on top of the research question data.

## Data Scope:
The data was collected in an interview style with 3 members of NAC interviewing one employee at a time and asking the three research questions above in a randomized pattern with randomized selection of the employees so that no single department was sampled more than another. The response from the interviewee was recorded in third person with the names of the employees recorded and then encoded in the final data. 
Along with the research questions and interviews, a post-interview survey was sent out asking everyone who was interviewed about their physical appearance (hair color), gender, corrective lenses usage, and hobbies. This data was stored within the code book where survey questions were joined to company data on employee names, age, years worked, and department. You can find keys to each numeric attribute within this codebook as well.

The three networks were compiled from edgelists where question data was converted into interaction data for the edgelist. This was then formatted using R to compile adjacency matrices for analysis. The R packages “xUCINET”, “sna”, and “igraph”, were used to collect statistical data on the networks in order to compare metrics like density, transitivity, dyadic and triadic relationships, and centrality. The networks were also visualized and information on how the network related to the information from the research questions was analyzed. 
