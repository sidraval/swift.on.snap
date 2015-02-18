//
//  Utils.swift
//  Humon
//
//  Created by Sid Raval on 2/18/15.
//  Copyright (c) 2015 Sid Raval. All rights reserved.
//

import Foundation

func catOptionals<T>(xs: [T?]) -> [T] {
    return xs.reduce([]) { accum, elem in
        if let x = elem {
            return accum + [x]
        }
        
        return accum
    }
}
