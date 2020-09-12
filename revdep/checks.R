library("revdepcheck")

#revdepcheck::revdep_reset()
revdepcheck::revdep_check(num_workers = 4)

revdepcheck::revdep_summary()        
revdepcheck::revdep_report_summary()
revdepcheck::revdep_report()
revdepcheck::revdep_report_problems()
revdepcheck::revdep_report_failures()
