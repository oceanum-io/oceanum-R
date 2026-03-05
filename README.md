# oceanum-R

R library for interacting with the [Oceanum.io platform](https://oceanum.io).

This is a R conversion of the [oceanum-python](../oceanum-python) library, providing read-only access to the Oceanum datamesh.

## Features

- Connect to the Oceanum datamesh API
- Browse and search the data catalog  
- Load datasources as MATLAB tables or NetCDF structures
- Execute OceanQL queries with filtering
- Internal staging for improved performance and size warnings
- Automatic session management  
- NetCDF support for dataset containers
- Query size limits with clear error messages
- Compatible with R version 4.5.1 and later

## Quick Start

```R
% Set your datamesh token (get from your Oceanum account)
sys.setenv(DATAMESH_TOKEN = "your_token")

% Create connection
connector = Connector$new();

% Browse catalog
catalog = connector$get_catalog('search');
disp(catalog);

% Load a datasource
data = connector$load_datasource('datasource-id');

% Make a query
query_input = list(datasource = 'datasource-id', limit = 1000)
result = connector$query(query_input);
```

## Documentation

The R API closely follows the Python library. For detailed documentation, see:
- [Python documentation](https://oceanum-python.readthedocs.io/) (API reference)
