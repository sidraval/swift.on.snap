#![Swift](https://raw.githubusercontent.com/sidraval/swift.on.snap/master/backend/assets/swift-logo.png)![Snap](https://raw.githubusercontent.com/sidraval/swift.on.snap/master/backend/assets/snap-logo-small.png)
An attempt to build a Swift powered iOS application backed by a RESTful [Snap](http://www.snapframework.com) backend.

## TODO
* Each service has its own `pg` snaplet embedded in it. Extract this.
* `withAuthorizedUser` should have type signature `Handler b v (Maybe User)` for generality.
  * Can't add restriction `(HasPostgres (Handler b v))` unfortunately...?
