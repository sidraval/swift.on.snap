import Foundation

func catOptionals<T>(xs: [T?]) -> [T] {
    return xs.reduce([]) { accum, elem in
        if let x = elem {
            return accum + [x]
        }
        
        return accum
    }
}
