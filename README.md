# paRdot
A simple R package for the Pardot API

### Install
```
devtools::install_github("sillson/paRdot")
```

### Getting Started:

Make sure to set your Pardot credentials
```
set_credentials('your-username', 'your-password', 'your-user-key')
```

Next, make a **paRdot** api call. Will return a XML response

```
pardot_client('object', 'operator', 'identifier_field', 'identifier')
```

### To Do:
- Extend to use optional request params, and JSON format
- Stick to one XML library
- Clean up string concatenation functions
- Extend to make pre-baked api calls that we can use to return lists, strings, etc. 
