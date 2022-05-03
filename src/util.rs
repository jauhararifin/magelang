use std::collections::{HashMap, HashSet};

use crate::errors::Error;

pub fn build_processing_plan<'a>(
    dependency_graph: &HashMap<&'a str, HashSet<&'a str>>,
) -> Result<Vec<&'a str>, Error> {
    if dependency_graph.is_empty() {
        return Ok(Vec::new());
    }

    let mut plan = Vec::new();
    let mut plan_set = HashSet::new();

    // TODO: make it more deterministic. maybe use ordered hash map for the iteration.
    for pack in dependency_graph.keys() {
        if plan_set.contains(pack) {
            continue;
        }

        let mut stack = vec![*pack];
        let mut in_stack = HashSet::new();
        in_stack.insert(*pack);

        while let Some(p) = stack.pop() {
            stack.push(p);
            let deps = dependency_graph.get(p).ok_or(Error::MissingPackage)?;

            let mut has_unresolved = false;
            for dep in deps.iter() {
                if plan_set.contains(dep) {
                    continue;
                }
                if in_stack.contains(dep) {
                    return Err(Error::ImportCycle);
                }

                stack.push(*dep);
                in_stack.insert(*dep);
                has_unresolved = true;
            }

            if !has_unresolved {
                plan.push(p);
                plan_set.insert(p);
                stack.pop();
            }
        }
    }

    Ok(plan)
}
