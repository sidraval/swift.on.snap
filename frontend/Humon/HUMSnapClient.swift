import Foundation
import Alamofire
import MapKit

class HUMSnapClient{
    let baseURL = "http://localhost:9000/api/"

    class var sharedClient: HUMSnapClient {
        struct Static {
            static let instance: HUMSnapClient = HUMSnapClient()
        }
        
        return Static.instance
    }
    
    init () {
        let headers = HUMUserSession.userIsLoggedIn() ? ["device-token": HUMUserSession.userToken()!] : ["device-token": NSUUID().UUIDString]
        Alamofire.Manager.sharedInstance.session.configuration.HTTPAdditionalHeaders = headers
    }
    
    func createCurrentUser(completionBlock: (NSError?) -> ()) {
        Alamofire.request(.POST, baseURL + "users").responseJSON { (_, _, JSON, error) -> Void in
            if let parsedJSON = JSON as? Dictionary<String, String> {
                HUMUserSession.setUserToken(parsedJSON["device_token"])
                HUMUserSession.setUserID(parsedJSON["id"])
            }
            
            dispatch_async(dispatch_get_main_queue(), { () -> Void in
                completionBlock(error)
            })
        }
    }
    
    func createEvent(event: HUMEvent, completionBlock: (String?, NSError?) -> ()) {
        Alamofire.request(.POST, baseURL + "events", parameters: event.properties()).responseJSON
        { (request, response, JSON, error) -> Void in
            println(response)
            if let parsedJSON = JSON as? Dictionary<String, String> {
                dispatch_async(dispatch_get_main_queue(), { () -> Void in
                    completionBlock(parsedJSON["id"], error)
                })
            }
        }
    }

    func fetchEventsIn(region: MKCoordinateRegion, completionBlock: ([HUMEvent], NSError?) -> ()) -> Alamofire.Request {
        let parameters = [
            "lat": region.center.latitude,
            "lon": region.center.longitude,
            "radius": region.span.latitudeDelta/2*111
        ]

        return Alamofire.request(.GET, baseURL + "events/nearests", parameters: parameters).responseJSON { (request, response, JSON, error) -> Void in
            if let parsedJSON = JSON as? [Dictionary<String, String>] {
                let possibleEvents = parsedJSON.map { (var eventJSON) -> HUMEvent? in
                    return HUMEvent.initWithJSON(eventJSON)
                }

                dispatch_async(dispatch_get_main_queue(), { () -> Void in
                    completionBlock(catOptionals(possibleEvents) as [HUMEvent], error)
                })
            } else {
                dispatch_async(dispatch_get_main_queue(), { () -> Void in
                    completionBlock([], error)
                })
            }
        }
    }
}
