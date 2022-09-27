let Job
    : Type
    = { organization : Text
      , position : Text
      , duration : Text
      , experiences : List Text
      }

let ContactInfo
    : Type
    = { name : Text, email : Text, github : Text }

let Project
    : Type
    = { name : Text, url : Text, description : List Text }

let Resume
    : Type
    = { contact : ContactInfo, history : List Job, projects : List Project }

let contact
    : ContactInfo
    = { name = "Jonathan Strickland"
      , email = "djanatyn@gmail.com"
      , github = "https://github.com/djanatyn"
      }

let history
    : List Job
    =   [ { organization = "Metafy"
          , position = "Senior Software Engineer"
          , duration = "June 2022 - September 2022"
          , experiences =
            [ "Backend Ruby development work using Hanami, Algolia, and GraphQL"
            , "Documented onboarding process to new codebase, highlighting challenges encountered in a shared \"Things I Got Stuck on in the Codebase\" document"
            , "Investigated and implemented enhancements to search indexes, enabling filtering coaches by additional criteria (character, position, popularity) in real-time"
            , "Laid off, along with 20% of the company, shortly after starting"
            ]
          }
        , { organization = "Recurse Center"
          , position = "Participant"
          , duration = "March 2022 - June 2022"
          , experiences =
            [ "Attended a self-directed educational retreat for 12 weeks to improve my skills as a programmer"
            , "Reverse-engineered Super Smash Brothers Melee GameCube, creating a Rust program \"melee-inject\" to build new executable game images with replaced character textures"
            , "Presented work in front of peers: <a href=\"https://docs.google.com/presentation/d/1sEnkbk3dOctiymV7YUATbzXb3zh2dj_D302XuHYNHi8/edit?usp=sharing\">\"Parsing and Transforming Super Smash Bros. Melee\"</a>"
            , "Presentation was featured as the first <a href=\"https://www.youtube.com/watch?v=KejJrmT590g\">\"Dispatches at RC\"</a> upload"
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

let projects
    : List Project
    =   [ { name = "melee-inject"
          , url = "https://github.com/djanatyn/melee-inject"
          , description =
            [ "Rust reverse-engineering project to replace character texture files in Super Smash Bros. Melee for the Nintendo GameCube"
            , "Transforms GCM filesystem table, replacing existing file entries and adjusting subsequent offsets, adding necessary padding"
            , "Uses `codegen` crate to generate structs (with doc comments) for every character's data files"
            ]
          }
        , { name = "fetch-followers"
          , url = "https://github.com/djanatyn/fetch-followers"
          , description =
            [ "Rust package to download account information from Twitter API, serializing to SQLite database"
            , "Uses tokio framework + message passing over channels to fetch data and update database (asynchronously)"
            , "Packaged using nix flakes, runs daily as a systemd oneshot unit on a timer"
            ]
          }
        , { name = "resume"
          , url = "https://github.com/djanatyn/resume"
          , description =
            [ "Haskell project to generate this resume! (demonstrating reproducible build principles)"
            , "Uses dhall for structured input data, blaze-html for generating markup, nix for managing build instructions + dependencies"
            , "pdf generated using wkhtmltopdf and custom stylesheet"
            ]
          }
        ]
      : List Project

let resume
    : Resume
    = { contact, history, projects }

in  resume
