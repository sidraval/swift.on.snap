//
//  HUMUserSession.swift
//  Humon
//
//  Created by Sid Raval on 2/16/15.
//  Copyright (c) 2015 Sid Raval. All rights reserved.
//

import SSKeychain

class HUMUserSession {
    class var HUMService: String { return "Humon" }
    class var HUMUserId: String { return "currentUserId" }
    class var HUMUserToken: String { return "currentUserToken" }
    
    class func userID() -> String? {
       let userID = SSKeychain.passwordForService(HUMService, account: HUMUserId)
        
        return userID
    }
    
    class func userToken() -> String? {
        let userToken = SSKeychain.passwordForService(HUMService, account: HUMUserToken)
        
        return userToken
    }
    
    class func setUserID(userID: String?) {
        if let userID = userID {
            SSKeychain.setPassword(userID, forService: HUMService, account: HUMUserId)
        } else {
            SSKeychain.deletePasswordForService(HUMService, account: HUMUserId)
        }
    }
    
    class func setUserToken(userToken: NSString?) {
        if let userToken = userToken {
            SSKeychain.setPassword(userToken, forService: HUMService, account: HUMUserToken)
        } else {
            SSKeychain.deletePasswordForService(HUMService, account: HUMUserToken)
        }
    }
    
    class func userIsLoggedIn() -> Bool {
        let hasUserId = (self.userID() != nil) ? true : false
        let hasUserToken = (self.userToken() != nil) ? true : false
        
        return hasUserId && hasUserToken
    }
}
