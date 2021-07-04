const pg = require('pg');

// Remove the default behaviour to parse some types into rich objects
// We keep two things:
// 1) representing PG NULL as JS null, to distinguish null from "null"
// 2) PG arrays being represented as JS arrays, because there doesn't seem
//    to be any way to disable this.
//    (Doing it this way is also slightly simpler, I think)
const id = x => x;
pg.types.setTypeParser(pg.types.builtins.DATE, id);
pg.types.setTypeParser(pg.types.builtins.TIMESTAMPTZ, id);
pg.types.setTypeParser(pg.types.builtins.TIMESTAMP, id);
pg.types.setTypeParser(pg.types.builtins.BOOL, id);

exports.new_f =
connectionString =>
async function()
{
  const client = new pg.Client({ connectionString });
  await client.connect();
  return client;
};

exports.query_f =
({ nullVal, arrayVal, otherVal, caseMaybeOf }) =>
db => sql => maybeVals =>
async function()
{
  let query = { text: sql, rowMode: 'array' };

  (caseMaybeOf(maybeVals)
    // v Nothing
    (() => {})
    // v Just vals
    (vals => () => query.values = vals)
  ());

  const result = await db.query(query);

  const rows = (
    Array.isArray(result)
      ? []  // Multiple statements executed, return nothing
      : result.rows
  );

  return rows.map(row => row.map(mapField));

  function mapField(val) {
    if (val === null) return nullVal;
    if (Array.isArray(val)) return arrayVal(val.map(mapField));
    if (typeof val !== 'string') throw Error('Value of unexpected type came from postgres');
    return otherVal(val);
  }
};
