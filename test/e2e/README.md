End to end Tests
----------------

This directory contains end-to-end integration tests for manipulating the database. It uses docker-compose to setup the database as a service and then performs queries and mutations to ensure everything is working correctly. The main things to test are:

 - Core functionality: queries and mutations are working.
 - Backward Compatibility: serialisation, queries and mutations work in the face of data upgrades.