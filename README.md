# ReviewR: A light-weight, portable tool for reviewing individual patient records

This is a shiny tool that allows for manual review of the MIMIC-III (https://mimic.physionet.org/) database when it has been uploaded into a Google Bigquery database. 

To function it requires a text file labeled: bigquery_projectid.txt that contains in plain text the name of your bigquery project that hosts the data. The project may host multiple databases, but the mimic data must be in a database titled "mimic3".

This is a work in progress and thus there are no guarantees of functionality or accuracy. Use at your own risk.
