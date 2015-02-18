//
//  HUMMapViewController.swift
//  Humon
//
//  Created by Sid Raval on 2/16/15.
//  Copyright (c) 2015 Sid Raval. All rights reserved.
//

import UIKit
import MapKit
import SVProgressHUD

class HUMMapViewController: UIViewController, MKMapViewDelegate {
    
    var mapView: MKMapView!

    override func viewDidLoad() {
        super.viewDidLoad()
        
        self.title = NSLocalizedString("Humon", comment: "")
        self.mapView = MKMapView(frame: self.view.frame)
        self.mapView.delegate = self
        
        self.view.addSubview(self.mapView)
        
        let button = UIBarButtonItem(barButtonSystemItem: UIBarButtonSystemItem.Add, target: self, action: "addButtonPressed")
        self.navigationItem.leftBarButtonItem = button
        // Do any additional setup after loading the view.
    }
    
    override func viewDidAppear(animated: Bool) {
        super.viewDidAppear(animated)
        
        if(!HUMUserSession.userIsLoggedIn()) {
            SVProgressHUD.show()
            HUMSnapClient.sharedClient.createCurrentUser({ (error) -> () in
                if let error = error {
                    SVProgressHUD.showErrorWithStatus("Authentication error")
                } else {
                    SVProgressHUD.dismiss()
                }
            })
        }
    }
    
    func addButtonPressed() {
        let coordinate = self.mapView.centerCoordinate
        let event = HUMEvent(name: "Name", address: "Address", startDate: NSDate(), endDate: NSDate(timeIntervalSinceNow: 100), lat: coordinate.latitude, lon: coordinate.longitude)
        
        let addEventTableViewController = HUMEventTableViewController.initWith(event, editable: true)
        self.navigationController?.pushViewController(addEventTableViewController, animated: true)
    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
        // Dispose of any resources that can be recreated.
    }
    

    /*
    // MARK: - Navigation

    // In a storyboard-based application, you will often want to do a little preparation before navigation
    override func prepareForSegue(segue: UIStoryboardSegue, sender: AnyObject?) {
        // Get the new view controller using segue.destinationViewController.
        // Pass the selected object to the new view controller.
    }
    */

}
