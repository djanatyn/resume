let Job
    : Type
    = { organization : Text
      , position : Text
      , duration : Text
      , experiences : List Text
      }

let ContactInfo
    : Type
    = { name : Text, email : Text }

let Resume
    : Type
    = { contact : ContactInfo, history : List Job }

let contact
    : ContactInfo
    = { name = "Jonathan Strickland", email = "djanatyn@gmail.com" }

let history
    : List Job
    =   [ { organization = "Metafy"
          , position = "Senior Software Engineer"
          , duration = "June 2022 - September 2022"
          , experiences =
            [ "Backend Ruby development work using Hanami, Algolia, and GraphQL"
            ]
          }
        , { organization = "Recurse Center"
          , position = "Participant"
          , duration = "March 2022 - June 2022"
          , experiences =
            [ "Reverse-engineered Super Smash Brothers Melee GameCube, creating a Rust program \"melee-inject\" to build new executable game images with replaced character textures"
            , "Presented work in front of peers: \"Parsing and Transforming Super Smash Bros. Melee\". Presentation was featured as the first \"Dispatches at RC\" upload"
            ]
          }
        , { organization = "American Eagle Outfitters"
          , position = "Systems Engineer"
          , duration = "December 2016 - January 2022"
          , experiences =
            [ "Built relationships with technology teams across the organization, working to understand needs, improving efficiency of code lifecycle and promotion pipeline"
            , "Installed, patched, maintained, and upgraded diverse infrastructure including Atlassian Bamboo, Bitbucket, Concourse CI, JIRA, JFrog Artifactory, Pivotal CloudFoundry, F5 BIG-IP LTM"
            , "Developed and maintained continuous integration pipelines for applications and provisioned infrastructure, empowering teams to create pull requests to apply changes, using Python, Bash, Rust, Haskell"
            , "Worked in multidisciplinary team for initial deployment of eCommerce API Gateway, solving performance and reliability problems along the way"
            , "Used Terraform to provision cloud environments, developing modules for different components of American Eagle infrastructure on AWS, GCP, GKE"
            , "First responder for on-call rotation, addressing production AE.com issues, failures in build + deployment automation"
            , "Used Terraform + Bamboo to create self-service \"ephemeral environments\" - when any UI developer opens a pull request, a new environment is launched, the new code is deployed, a DNS record is set up, the developer is sent a link to the live environment on their pull request to validate"
            , "Received many awards and nominations from peers and managers, including 2019 Eagles Elite \"in recognition of your determination to go above and beyond at AEO and exemplifying innovation and teamwork\""
            ]
          }
        , { organization = "Industry Weapon"
          , position = "Contractor"
          , duration = "December 2015 - June 2016"
          , experiences =
            [ "Evangelized greater use of version control, unit tests, documentation among my teams, resulting in a self-hosted gogs (go git service) instance (eventually was used by all development teams)"
            , "Developed and shipped many custom software integrations for our digital signage platform using modern development practices to meet customer needs, regularly automating time-consuming parts of the development process"
            , "Used Flask to develop secure, RESTful APIs with token-based authentication to serve as a backend for web applications"
            , "Automated deployment process for Python applications using single executable with dependencies and application code self-contained inside"
            , "Identified, documented, began discussions about RCE vulnerabilities in core parts of our codebase"
            , "Worked with other developers to fix bugs in a decade old PHP web application using Bugzilla"
            ]
          }
        , { organization = "Cytobank"
          , position = "System Administrator"
          , duration = "January 2014 - August 2015"
          , experiences =
            [ "Used Puppet and Ruby scripting extensively to automate configuration of dev and prod servers"
            , "Participated in regular on-call rotations, responded to production outages"
            , "Worked on a team of 3 to facilitate migration of customers from physical OpenVZ hosting provider to AWS, organizing downtime, executing migration for most hosts"
            , "Educated rest of team on infrastructure through meetings, recorded videos, thorough documentation"
            ]
          }
        ]
      : List Job

let resume
    : Resume
    = { contact, history }

in  resume
