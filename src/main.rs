use std::{fs, process::Command};

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};

#[derive(Debug, Deserialize, Serialize)]
struct Resume {
    contact: ContactInfo,
    skills: Vec<Skill>,
    history: Vec<Job>,
}

#[derive(Debug, Deserialize, Serialize)]
struct Skill(String);

#[derive(Debug, Deserialize, Serialize)]
struct ContactInfo {
    name: String,
    email: String,
    github: String,
    website: String,
}

#[derive(Debug, Deserialize, Serialize)]
struct Job {
    organization: String,
    position: String,
    duration: String,
    experiences: Vec<Vec<Inline>>,
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(tag = "type", content = "value")]
enum Inline {
    Text(String),
    Code(String),
    Link { text: String, url: String },
}

fn main() -> Result<()> {
    fs::create_dir_all("build").context("creating build directory")?;

    let ron = fs::read_to_string("resume.ron").context("reading resume.ron")?;
    let resume: Resume = ron::from_str(&ron).context("parsing resume.ron")?;

    let json = serde_json::to_string_pretty(&resume).context("serializing resume JSON")?;
    fs::write("build/resume.json", json).context("writing build/resume.json")?;

    let status = Command::new("typst")
        .args(["compile", "typst/template.typ", "build/resume.pdf"])
        .status()
        .context("running typst")?;

    anyhow::ensure!(status.success(), "typst compile failed");

    Ok(())
}
