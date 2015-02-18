import Foundation

extension NSDateFormatter {
    class func RFC3339DateFormatter() -> NSDateFormatter {
        let dateFormatter = NSDateFormatter()
        let locale = NSLocale(localeIdentifier: "en_US_POSIX")
        dateFormatter.locale = locale
        dateFormatter.dateFormat = "yyyy'-'MM'-'dd'T'HH':'mm':'ss'.'SSS'Z'"
        dateFormatter.timeZone = NSTimeZone(forSecondsFromGMT: 0)

        return dateFormatter
    }
}
