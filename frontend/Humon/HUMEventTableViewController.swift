//
//  HUMEventTableViewController.swift
//  Humon
//
//  Created by Sid Raval on 2/16/15.
//  Copyright (c) 2015 Sid Raval. All rights reserved.
//

import UIKit
import SVProgressHUD

class HUMEventTableViewController: UITableViewController {
    
    let HUMEventCellIdentifier = "HUMEventCellIdentifier"
    private var event: HUMEvent?
    private var editable: Bool?
    
    enum Cells: Int {
       case EventCellName = 0, EventCellAddress, EventCellStart, EventCellEnd, EventCellSubmit, EventCellCount
    }
    
     class func initWith(event: HUMEvent, editable: Bool) -> HUMEventTableViewController {
        let tableView = HUMEventTableViewController(style: UITableViewStyle.Plain)
        tableView.event = event
        tableView.editable = editable

        return tableView
    }

    override func viewDidLoad() {
        super.viewDidLoad()
        
        self.tableView.registerClass(UITableViewCell.self, forCellReuseIdentifier: HUMEventCellIdentifier)
    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
    }

    // MARK: - Table view data source

    override func numberOfSectionsInTableView(tableView: UITableView) -> Int {
        return 1
    }

    override func tableView(tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return Cells.EventCellCount.rawValue
    }
    
    override func tableView(tableView: UITableView, cellForRowAtIndexPath indexPath: NSIndexPath) -> UITableViewCell {
        let cell = tableView.dequeueReusableCellWithIdentifier(HUMEventCellIdentifier, forIndexPath: indexPath) as UITableViewCell
        
        return cell
    }
    
    override func tableView(tableView: UITableView, didSelectRowAtIndexPath indexPath: NSIndexPath) {
        if (indexPath.row != Cells.EventCellSubmit.rawValue) {
            return
        }
        
        SVProgressHUD.show()
        
        if let event = self.event {
            HUMSnapClient.sharedClient.createEvent(event, completionBlock: { (eventID, error) -> () in
                if let error = error {
                    SVProgressHUD.showErrorWithStatus("Authentication error")
                } else {
                    SVProgressHUD.dismiss()
                    self.navigationController?.popToRootViewControllerAnimated(true)
                }
            })
        }
    }

}
