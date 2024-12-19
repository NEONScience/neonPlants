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
- Create a fork of this repo and make updates in your fork
- Function names should be camelCase and use the "verbNoun" format, e.g., `stackPlantPresence`
- Arguments for functions should also use camelCase.
- Default data inputs are assumed to be a list object from the `neonUtilities` package, unless otherwise noted in the function documentation.
- input arguments that are lists should be `inputDataList` or formatted with the data product module code if a specific data product is required, e.g., `inputDataListVst`
- Some functions may have use cases where dataframe inputs might be useful as an alternative. These arguments should be camelCase, and are assumed to be dataframes if there is no "List" in the name, e.g., `inputCore`.
- When your updates are ready for review, submit your pull request to the `dev` branch. 

