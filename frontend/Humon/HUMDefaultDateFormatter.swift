//
//  HUMDefaultDateFormatter.swift
//  Humon
//
//  Created by Sid Raval on 2/17/15.
//  Copyright (c) 2015 Sid Raval. All rights reserved.
//

import Foundation

extension NSDateFormatter {
    class func RFC3339DateFormatter() -> NSDateFormatter {
        let dateFormatter = NSDateFormatter()
        let locale = NSLocale(localeIdentifier: "en_US_POSX")
        dateFormatter.locale = locale
        dateFormatter.dateFormat = "yyyy'-'MM'-'dd'T'HH':'mm':'ss'.'SSS'Z'"
        dateFormatter.timeZone = NSTimeZone(forSecondsFromGMT: 0)

        return dateFormatter
    }
}