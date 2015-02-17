//
//  HUMEvent.swift
//  Humon
//
//  Created by Sid Raval on 2/17/15.
//  Copyright (c) 2015 Sid Raval. All rights reserved.
//
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
    
    class func initWithJSON(JSON: [[String: String]]) -> HUMEvent? {
        return .None
    }
}