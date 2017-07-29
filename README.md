# JSONs to Schema

**A JSON Schema Draft 4 Generator from JSON Instances**

Suppose you have an API that produces complex and somewhat varied JSON. You'd like to be able to document that API in a standard format by observing only its output. [JSON Schema](http://json-schema.org/) is a reasonable format for that. But simply generating the schema for one document will lead to an incomplete description of the API since not all documents will have all the keys in the full document. This is where this library comes in.

## CLI Overview

JSONs to Schema can also be used on the command line. After locating the installed binary, run it with `--help` to get detailed usage instructions.

## API Overview

The main API function is `jsonsToSchema` along with its configuration-taking equivalent, `jsonsToSchemaWithConfig`. This will transform a set of JSON documents into a single `Maybe` schema. 

Other helpful functions are `jsonToSchema`, which transforms a single document, and `schemasToSchema`, which performs the actual work of schema unification. 

## Properties

The schema generator should satisfy these properties:

- An schema generated from a JSON document will always be able to validate that JSON document.
- When a schema is merged with another schema, all objects that would validate under either schema, subject to the assumptions below, will validate under the merged schema regardless of merge order.

Unlike other generators strictness is meant to be customizable. Whether to allow additional properties in schema objects is something that the user can turn on and off.

### Assumptions
When merging JSON documents the generator keeps track of only one possible underlying type for each primitive type. Thus, if there were two documents `{"name": "x"}` and `{"cost": 24}` they will be assumed to represent two different manifestations of a single document that has both the `name` and the `cost` keys, rather than two different documents.

If given an object and an array (or some other primitive type), however, the generator will be able to assume that the particular JSON document can be either an object or an array type. 

## Prior Art

This project was inspired by [GenSON](https://github.com/wolverdude/GenSON), a Python project that does much of the same, but aims to be more customizable, correct, and powerful. 

Certain features were also inspired by [schema-guru](https://github.com/snowplow/schema-guru), but this project aims to be more customizable and have a programmatic API. Schema Guru's enum and range detection features are planned to be matched by this library. 

## Future Plans
- Option to autogenerate enumerations for fields with below a certain limit of values
- Option to automatically detect ranges for integers, strings, arrays, and objects


## Not Planned
- Recognition of known JSON Schema formats (UUIDs and IP addresses, for example)
- Support for JSON or JSON Schema extensions
