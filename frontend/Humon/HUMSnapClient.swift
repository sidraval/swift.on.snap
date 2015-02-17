//
//  HUMSnapClient.swift
//  Humon
//
//  Created by Sid Raval on 2/16/15.
//  Copyright (c) 2015 Sid Raval. All rights reserved.
//

import Foundation
import Alamofire

class HUMSnapClient {
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
        Alamofire.request(.POST, "http://localhost:9000/api/users").responseJSON { (_, _, JSON, error) -> Void in
            if let parsedJSON = JSON as? Dictionary<String, String> {
                HUMUserSession.setUserToken(parsedJSON["device_token"])
                HUMUserSession.setUserID(parsedJSON["id"])
            }
            
            dispatch_async(dispatch_get_main_queue(), { () -> Void in
                completionBlock(error)
            })
        }
    }
}