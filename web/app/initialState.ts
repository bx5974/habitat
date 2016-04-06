import {List, Map, Record} from "immutable";

export default Record({
    app: Record({
        name: "Habitat",
        currentYear: new Date().getFullYear(),
    })(),
    gitHub: Record({
        isLinked: false,
        repos: List(),
        selectedOrg: undefined,
        username: undefined,
    })(),
    notifications: Record({
        all: List(),
    })(),
    orgs: Record({
        added: List(),
        all: List(),
        current: Record({
            namespace: undefined,
            name: undefined,
            email: undefined,
            website: undefined,
            members: List(),
            availableMemberSearchResults: List([
                Record({
                    username: "testUser",
                    name: "Test User",
                    email: "smith+chef-logo@getchef.com",
                    status: "",
                    canBeAdded: true,
                    ui: Record({
                        isActionsMenuOpen: false
                    })(),
                })(),
                Record({
                    username: "testUser2",
                    name: "Test User 2",
                    email: "nlloyds@gmail.com",
                    status: "",
                    canBeAdded: true,
                    ui: Record({
                        isActionsMenuOpen: false
                    })(),
                })(),
            ]),
            memberSearchResults: List(),
        })(),
        ui: Record({
            create: Record({
                saved: false,
            })(),
        })(),
    })(),
    packages: Record({
        current: undefined,
        explore: List(),
        visible: List(),
        ui: Record({
            current: Record({
                errorMessage: undefined,
                exists: false,
                loading: true,
            })(),
            visible: Record({
                errorMessage: undefined,
                exists: false,
                loading: true,
            })(),
        })(),
    })(),
    projects: Record({
        // This is a temporary hack that lets us add projects, and gets
        // concatted with projects on display. In real life we'll make another
        // server call when displaying a list after a project is added and it will
        // be there
        added: List(),
        all: List(),
        current: Record({
            origin: undefined,
            name: undefined,
            description: undefined,
            latestBuild: undefined,
            sourceUrl: undefined,
            maintainer: Record({

            })(),
            sourceRepository: Record({

            })(),
            builds: List(),
            buildLogs: Map(),
            ui: Record({
                exists: false,
                loading: true,
            })()
        })(),
    })(),
    router: Record({
        requestedRoute: "",
        route: "",
    })(),
    users: Record({
        current: Record({
            email: undefined,
            isSignedIn: false,
            isUserNavOpen: false,
            username: undefined,
        })(),
    })(),
})();
