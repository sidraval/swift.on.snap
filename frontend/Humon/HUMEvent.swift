import Foundation
import MapKit


class HUMEvent: NSObject, MKAnnotation {
    let name: String
    let address: String
    let startDate: NSDate?
    let endDate: NSDate?
    let coordinate: CLLocationCoordinate2D
    var userID: Int?
    var eventID: Int?

    init(name: String, address: String, startDate: NSDate, endDate: NSDate, lat: CLLocationDegrees, lon: CLLocationDegrees) {
        self.name = name
        self.address = address
        self.startDate = startDate
        self.endDate = endDate
        self.coordinate = CLLocationCoordinate2DMake(lat, lon)
    }
    
    class func initWithJSON(JSON: Dictionary<String,String>) -> HUMEvent? {
        if let name = JSON["name"] {
            if let address = JSON["address"] {
                if let startDate = JSON["startDate"] {
                    if let startDate = NSDateFormatter.RFC3339DateFormatter().dateFromString(startDate) {
                        if let endDate = JSON["endDate"] {
                            if let endDate = NSDateFormatter.RFC3339DateFormatter().dateFromString(endDate) {
                                if let lat = JSON["lat"] {
                                    if let lon = JSON["lon"] {
                                        return HUMEvent(name: name, address: address, startDate: startDate, endDate: endDate, lat: (lat as NSString).doubleValue, lon: (lon as NSString).doubleValue)
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        
        return .None
    }

    func properties() -> Dictionary<String, AnyObject> {
        if let endDate = endDate {
            if let startDate = startDate {
                return [
                    "event.eventEndedAt": NSDateFormatter.RFC3339DateFormatter().stringFromDate(endDate),
                    "event.eventStartedAt": NSDateFormatter.RFC3339DateFormatter().stringFromDate(startDate),
                    "event.eventName": name,
                    "event.eventAddress": address,
                    "event.eventLat": coordinate.latitude,
                    "event.eventLon": coordinate.longitude
                ]
            }
        }

        return [:]
    }
    
    class func eventsWithJSON(JSON: [Dictionary<String, String>]) -> [HUMEvent] {
        return catOptionals(JSON.map { (var json) -> HUMEvent? in
            return self.initWithJSON(json)
        })
    }
}
