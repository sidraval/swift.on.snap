#![Swift](https://raw.githubusercontent.com/sidraval/swift.on.snap/master/backend/assets/swift-logo.png)![Snap](https://raw.githubusercontent.com/sidraval/swift.on.snap/master/backend/assets/snap-logo-small.png)
An attempt to build a Swift powered iOS application backed by a RESTful [Snap](http://www.snapframework.com) backend.

## Building the backend
```bash
$ git clone https://www.github.com/sidraval/swift.on.snap.git
$ cd swift.on.snap/backend
$ cabal sandbox init
$ cabal install snap
$ cabal install
$ backend -p 9000
```

## To do
* Each service has its own `pg` snaplet embedded in it. Extract this.
* `withAuthorizedUser` should have type signature `Handler b v (Maybe User)` for generality.
  * Can't add restriction `(HasPostgres (Handler b v))` unfortunately...?
* Proper JSON parsing in Swift, rather than `Dictionary<String, String>`
* Snap should return proper JSON (i.e. not every value should be a string), so we don't have to cast in Swift.
* Use preprocessor macros for URLs instead of hardcoding
