Miragrations
------------

The store handles database migrations itself, but we only support adding fields with default
values. This seems like a safe, useful and hopefully not too limiting approach.

The `db` directory contains a database that we version control with Git so we can check two
important properties:

 - We can write new and read old values even with a new version of the types.
 - We do not break hashing (content-addressing)!

Every time we update the types to add a new field, we should update this test too. We need to add
a new entry for the value, read all of the old values and check content-addressing. We have added
commands in the CLI to make this very simple and MDX-able.

## Add a new entry

```
retirement dummy | retirement begin-tx --directory=./db
echo "ABCDEFG" | retirement complete-tx --directory=./db --hash=<hash>
```

## Entries

```json
{
  "v0_0_1": "1220b4dcf753472077948c277f0bdf4b04a93187f6996b929f2d7f98ff32c05bb3ee"
}
```

## Tests

### Reading all values just works

```sh
$ retirement dump --directory=./db
{"version":{"major":0,"minor":1},"ts":"2023-02-15T22:02:33-00:00","details":{"flightDetails":[{"date":"2023-02-09t5:48:10.801Z","departure":{"id":"BFS","name":"Belfast","iataCode":"BFS"},"arrival":{"id":"LHR","name":"London","iataCode":"LHR"},"passengerCount":1,"flightCount":1}],"trainDetails":[],"taxiDetails":[],"additionalDetails":[],"primaryReason":"Conference","reasonText":"Some reason for travelling!"},"booker_crsid":"xyz123","business_traveller":{"crsid":"abc123","name":"Alice","department":"CST"},"financeKind":"Grant","offset":{"tokenId":1234,"projectName":"Gola","minter":"abcd1234wxyz5678","kyc":"1234abcd5678wxyz","amount":556789},"tx_id":"ABCDEFG\n","grantDetails":{"sponsorAndPiConfirmation":true,"award":"award","project":"project","task":"task"}}
```

### Hash lookup still works

If you add a new value, add another line to this test!

```sh
$ retirement lookup --directory=./db --hash=1220b4dcf753472077948c277f0bdf4b04a93187f6996b929f2d7f98ff32c05bb3ee
{"version":{"major":0,"minor":1},"ts":"2023-02-15T22:02:33-00:00","details":{"flightDetails":[{"date":"2023-02-09t5:48:10.801Z","departure":{"id":"BFS","name":"Belfast","iataCode":"BFS"},"arrival":{"id":"LHR","name":"London","iataCode":"LHR"},"passengerCount":1,"flightCount":1}],"trainDetails":[],"taxiDetails":[],"additionalDetails":[],"primaryReason":"Conference","reasonText":"Some reason for travelling!"},"booker_crsid":"xyz123","business_traveller":{"crsid":"abc123","name":"Alice","department":"CST"},"financeKind":"Grant","offset":{"tokenId":1234,"projectName":"Gola","minter":"abcd1234wxyz5678","kyc":"1234abcd5678wxyz","amount":556789},"grantDetails":{"sponsorAndPiConfirmation":true,"award":"award","project":"project","task":"task"}}
```
