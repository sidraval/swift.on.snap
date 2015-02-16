//
//  HUMMapViewController.swift
//  Humon
//
//  Created by Sid Raval on 2/16/15.
//  Copyright (c) 2015 Sid Raval. All rights reserved.
//

import UIKit
import MapKit

class HUMMapViewController: UIViewController, MKMapViewDelegate {
    
    var mapView: MKMapView!

    override func viewDidLoad() {
        super.viewDidLoad()
        
        self.title = NSLocalizedString("Humon", comment: "")
        self.mapView = MKMapView(frame: self.view.frame)
        self.mapView.delegate = self
        
        self.view.addSubview(self.mapView)
        // Do any additional setup after loading the view.
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
