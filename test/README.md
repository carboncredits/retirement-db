Transaction Store CLI
---------------------

The transaction store has a CLI for managing the store. 

```sh
$ retirement --version
n/a
```

We can get some dummy content using the CLI tool. We'll use a timestamp for reproducibility
in these tests but you could just leave it out and the current time will be used instead.

```sh
$ retirement dummy --timestamp="2022-12-09T20:27:07-00:00"
{
  "version": { "major": 0, "minor": 1 },
  "ts": "2022-12-09T20:27:07-00:00",
  "details": {
    "flightDetails": [],
    "trainDetails": [],
    "taxiDetails": [],
    "additionalDetails": [],
    "primaryReason": "Conference",
    "reasonText": "Some reason for travelling!"
  },
  "id": { "crsid": "abc123", "name": "Alice", "department": "CST" },
  "financeKind": "Grant",
  "offset": {
    "tokenId": 1234,
    "projectName": "Gola",
    "minter": "abcd1234wxyz5678",
    "kyc": "1234abcd5678wxyz",
    "amount": 556789
  },
  "grantDetails": {
    "sponsorAndPiConfirmation": true,
    "award": "award",
    "project": "project",
    "task": "task"
  }
}
```

We can then start a transaction in the store i.e. store some private data and get the hash!

```sh
$ retirement dummy --timestamp="2022-12-09T20:27:07-00:00" | retirement begin-tx --directory=./test 
1220d7e7ab75fdff1f5cc70f69dc5c2f02a08adcc2f8d0dd5c3060bdfea3a3ad6f79
```

We can't add the same bit of data twice, our store relies on every value being unique.

```sh
$ retirement dummy --timestamp="2022-12-09T20:27:07-00:00" | retirement begin-tx --directory=./test 
Failed to store!
```

After that, at any point, we can check the status of the value by its hash.

```sh
$ echo 1220d7e7ab75fdff1f5cc70f69dc5c2f02a08adcc2f8d0dd5c3060bdfea3a3ad6f79 | retirement check-tx --directory=./test
PENDING
```

We can then manually complete the transaction.

```sh
$ echo "ABCDEFG" | retirement complete-tx --directory=./test --hash=1220d7e7ab75fdff1f5cc70f69dc5c2f02a08adcc2f8d0dd5c3060bdfea3a3ad6f79 | grep -o SUCCESS
SUCCESS
```

Just grepping in order to remove the commit hash that's returned to make the tests reproducible.

And check the status again

```sh
$ echo 1220d7e7ab75fdff1f5cc70f69dc5c2f02a08adcc2f8d0dd5c3060bdfea3a3ad6f79 | retirement check-tx --directory=./test
COMPLETE: ABCDEFG
```