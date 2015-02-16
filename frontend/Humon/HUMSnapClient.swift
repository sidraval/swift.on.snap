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
        Alamofire.Manager.sharedInstance.session.configuration.HTTPAdditionalHeaders = ["device-token": NSUUID().UUIDString]
    }
    
    func createCurrentUser(completionBlock: (NSError) -> ()) {
        Alamofire.request(.POST, "http://localhost:9000/api/users").responseJSON { (_, _, _, error) -> Void in
            if let error = error {
                completionBlock(error)
            }
        }
    }
}