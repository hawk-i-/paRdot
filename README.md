# paRdot
A simple R package for the Pardot API

### Install
```
devtools::install_github("hawk-i-/paRdot")
```

### Getting Started:

Make sure to set your Pardot credentials
```
set_credentials('your-username', 'your-password', 'your-user-key')
```

Next, make a **paRdot** api call. Will return a dataframe

**Prebaked dataframes:**

```
all_prospects_dataframe <- pardot_prospects()
```

```
all_campaigns_dataframe <- pardot_campaigns()
```

**XML response with more configuration:**

```
your_dataframe <- pardot_client('object', 'operator', 'identifier_field', 'identifier')
```

### To Do:
- Extend to use optional request params, and JSON format
- Stick to one XML library
- Extend to make pre-baked api calls that we can use to return lists, strings, etc. 
