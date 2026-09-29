neonPlants
================

<!-- ****** Description ****** -->

Library of R functions to work with NEON plant data

To install the package:
`remotes::install_github("NEONScience/neonPlants")`

<!-- ****** Requests ****** -->
Requests
--------

Bug reports and feature requests should be posted to the <a href="https://github.com/NEONScience/neonPlants/issues"> Issues page</a> in this repository.


<!-- ****** Usage ****** -->
Usage
-----

Documentation is in development. 

If you encounter problems with code in this repository, feel free to post an issue.

<!-- ****** Acknowledgements ****** -->
Credits & Acknowledgements
--------------------------

<!-- Acknowledgements text -->
The National Ecological Observatory Network is a project solely funded by the National Science Foundation and managed under cooperative agreement by Battelle. Any opinions, findings, and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the National Science Foundation.

<!-- ****** License ****** -->
License
-------
 GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

<!-- ****** Disclaimer ****** -->
Disclaimer
----------
*Information and documents contained within this repository are available as-is. Codes or documents, or their use, may not be supported or maintained under any program or service and may not be compatible with data currently available from the NEON Data Portal.*

<!-- ****** Contributing ****** -->
Contributing to the package
----------
- Create a branch within this repo and make updates in your branch.
- Function names should be camelCase and use the "verbNoun" format, e.g., `stackPlantPresence`
- Arguments for functions should also use camelCase.
- Default data inputs are assumed to be a list object from the `neonUtilities` package, unless otherwise noted in the function documentation.
- When your updates are ready for review, submit your pull request to the `dev` branch. 

Code development guidelines
----------
Notes come from Q1 meeting of function authors:
- Functions should accept lists as input as downloaded from `neonUtilities::loadByProduct()` as well as individual tables as separate input arguments if a list is not provided by the user.
- Use `inputDataList` as default list input argument name.
- Use `input[Table]` as table input argument names - e.g., 'inputCore' for 'bbc_percore' table input to root functions.
- Implement error handling to provide useful feedback to end-users when both lists and tables are supplied, when lists that lack required tables are supplied, when tables are supplied that lack required columns, when tables lack data (either provided within a list or separately), etc.
- Implement testing using `testthat` to verify, at minimum, that:
   - Using test dataset, output data are expected type and dimensions. Can also evaluate whether specific anticipated values are returned after processing test dataset, if appropriate.
   - All errors are generated as expected - e.g., list and table input error handling.

Additional thoughts from SS/CM discussion:
- Avoid using hyphens in any new column names that are created, as these require special handling via `` characters and are needlessly difficult for end-users.
- Column names with units or data that are already defined in the NEON "Variables" file that comes with `neonUtilities` downloads should be re-used whenever possible.
- Columns with new derived data/units should have names that include the units, separated via an "_". _Example_: `dryMass_Mgha`.
- Any function producing biomass as an output should include a column with units "Mg per hectare" with separate plot- and site-level table outputs. This will enable the biomass umbrella function to easily incorporate results.
- The site-level output table should have mean, sd, and plotNum columns - e.g., 'rootMassMean_Mgha', 'rootMassSD_Mgha', 'rootPlotNum'.

Documentation guidelines
----------
- @title: First word capitalized, other words lower-case. Title should succinctly state what function does.
- @description: A 1-2 sentence high-level summary of required inputs and what the function produces with them. Include a data-product-specific modification of this statement: "Data inputs are [NEON data product name] (DP1.#####.001) retrieved using the neonUtilities::loadByProduct() function (preferred), data downloaded from the NEON Data Portal, or input data tables with an equivalent structure and representing the same site x month combinations."
- @details: Summarize those details necessary for a user to understand required function inputs and effectively work with function output. For example, if there are duplicates in the input data, how does the function handle them? If there are analytical replicates, how does the function process them? Are lower-level quality flags preserved or filtered out if a higher-level calculation is made?
- @params general guidance: Ensure all function input arguments have an @param entry. Ensure all @param entries in the function header text have a corresponding function argument; testing will break if this is not true.
- @param inputDataList: Modify the following to be data-product specific: "A list object comprised of [NEON data product name] tables tables (DP1.#####.001) downloaded using the neonUtilities::loadByProduct function. If list input is provided, the table input arguments must all be NA; similarly, if list input is missing, table inputs must be provided. Add any other necessary details... [list]" 
- @param input[Table]: Modify the following to be data-product specific: "The 'mod_tableName' table for the site x month combination(s) of interest. (defaults to NA). If table input is provided, the 'inputDataList' argument must be missing. Add any other necessary details... [data.frame]"
- @return: Describe the object the function returns - i.e., list, data frame, etc. Additionally, describe any new columns created that are not documented in the NEON "Variables" file, including units as appropriate.
- @examples: Include a call to `neonUtilities::loadByProduct()` as a first step to reinforce with users that this is the ideal way to provide data to the function.
