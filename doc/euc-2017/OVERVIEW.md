- Introduction
    * Me, works at Miniclip
- Bugs
    * Bugs right? We all have them
        * This one time at Pool
            * Tell people about the tier count bug that went negative
              and kicked everybody out of the game
    * We really don't have a choice, we will always have bugs and we will always need to fix them
        * Stop the server, update code, start again -> Sad face, now you're costing money to the company while
          they wait for you to fix *your* bug 
        * Blue/Green deployments, maybe you're using a load balancer, you remove the server from it and let it drain out, then stop, update and
          start it again -> better than the previous option but you don't want to be going this manually when running hundreds/thousands
          of servers, you can script it but that draining bit is gonna take a long time
- Hot code loading
    * But wait, Erlang allows us to load new code without stopping the service
        * This is an invaluable feature for business
        * And a lot simpler
    * So how do we go about it?
        * build the new beam version of the module you want to replace
        * copy the new version on top of the old one
        * attach to a running node, do l(module).
        * done, the new code is running now
    * Messy business
        * you have to manually calculate dependencies before loading code
            * module A calls method from module B that in turn calls method from module C, aaargh!  
        * your app thinks its running version x.y.z but it's really not
        * if you change an option in your sys.config you must remember to set that option at the console
          and then update the file on disk
        * you better be really disciplined when applying these changes or else it get confusing really fast
    * But there is a bright side to it
        * you can apply your changes incrementally
        * canarying deployments should be standard practice
        * always remember: bug fixes can have bugs themselves, sometimes nastier that the original ones
          (especially when in a rush)
- Simple load module
        * code_server handles code related things
        * pointer to old version, current version, processes are initially running current version, only when
          making qualified function calls does the new code get a chance of being loaded
        * gen_server does it
        * l(module).
            * code:purge
            * code:load_file
- Release upgrades
    * The structured way of hot code loading
        * Much more powerful than simply loading modules at the console
            * Tied in with OTP's infrastructure
                * gen_server/fsm code_change to migrate state
                * starts new applications
                * reloads sys.config and warns applications of changes made to it
        * Downside: really complicated
            * LYSE describes relups as the 9th circle of Erl (http://learnyousomeerlang.com/relups#upgrading-the-release)
            * Fred offers you a break on page ??? but i guarantee it you'll be wanting it way before
    * Release upgrade workflow
        * .appup
        * relup
        * apply upgrade
    * It begins with an .appup file
        * An "application upgrade file defines how an application is upgraded or downgraded in a running system" (http://erlang.org/doc/man/appup.html)
        * A single term containing instructions describing how to upgrade/downgrade from one specific version to another
            * {Vsn,
                [{UpFromVsn, Instructions}, ...],
                [{DownToVsn, Instructions}, ...]}.
            * {"2.0",
                [{"1.9", Instructions}, ...],
                [{"1.9", Instructions}, ...]}.
            * you can use regular expressions for the version strings (eg. <<"2\\.1\\.[0-9]+">>)
        * The instructions available to you are:
            * load_module
                * simple replacement of a module with fresh code
                * {load_module, Mod, PrePurge, PostPurge, DepMods}
                    * PrePurge/PostPurge can either be soft or brutal
                    * DepMods is a list of modules that should be loaded before this one
            * add_module
                * Loads a new module
                * {add_module, Mod, DepMods}
            * delete_module
                * Loads a new module
                * {delete_module, Mod, DepMods}
            * add_application
                * Adds and starts a new application
                * {add_application, Application, Type}
            * remove_application
                * {remove_application, Application}
            * restart_application
            * update
                * synchronized update of processes running intended module
                    * this means suspending, applying the upgrade and then resuming all processes running the module
                    * achieved by obtaining all supervised processes (ie. recursively searching down from the main supervisor),
                      iterating through them and asking that they suspend themselves (ie. sys:suspend).
                        * CAVEAT: some new processes might pop up while doing this, they won't be suspended
                * gen_* all have the code_change/3,4 callback
                    * Module:code_change(OldVsn, State, Extra)
                    * it allows you to migrate you state structure, when this method is called you get the old state (while running new code)
                      and return new state.
                    * if you're using records to store your state it's a real PITA to migrate it
                    * the Extra argument is additional data fed directly from the appup for custom processing
                * {update, Mod, ModType, Timeout, Change, PrePurge, PostPurge, DepMods}
                    * ModType, either static or dynamic, static means ????
                    * Change is either soft or {advanced, Extra}
                        * that's where the Extra argument in code_change comes from
                    * Timeout is the time allowed to wait for the suspend, remove, code_change requests
                    * PrePurge, PostPurge
                    * DepMods
    * Next comes the relup file
        * Similar to the appup but at a lower level, you tipically don't need to mess with this one
    * The upgrade itself is just asking release_handler to upgrade to the tarball you have somewhere with the new version
    * Doing it by hand (short version)
        * Writing the appup
        * Give it to systools and ask it to create a relup file
            * systools:make_relup
            * systools:make_tar
        * Unpacking and upgrading the release
            * release_handler:unpack_release
            * release_handler:which_releases
            * release_handler:install_release
            * release_handler:make_permanent
- Automation (using rebar3)
    * There are some plugins already that take away some of the manual work
        * erlup (https://github.com/soranoba/erlup)
        * relflow (https://github.com/RJ/relflow)
        * rebar3_appup_plugin (https://github.com/lrascao/rebar3_appup_plugin)
    * I'll be talking about rebar3_appup_plugin (which is the one i wrote)
    * Appup generation
        * 


