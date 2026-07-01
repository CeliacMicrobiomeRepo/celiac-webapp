# Automated Deployment Setup

This document explains how to configure GitHub Actions to automatically refresh data from the Celiac Microbiome Repository and redeploy the webapp.

## What It Does

1. **Weekly refresh**: Every Monday at 00:00 UTC, the webapp fetches the latest data from `celiac-repository` and redeploys to shinyapps.io.
2. **On data change**: When someone pushes changes to data files in `celiac-repository`, it triggers an immediate redeploy of the webapp.
3. **Manual trigger**: You can trigger a redeploy anytime from the Actions tab.

## Setup Steps

### 1. Get shinyapps.io credentials

From your shinyapps.io dashboard (logged in as the "celiac" account):

- Go to **Account** → **Tokens**
- Copy your **Token** and **Secret**

### 2. Add secrets to the `celiac-webapp` repository

Go to **Settings** → **Secrets and variables** → **Actions** → **New repository secret**:

| Secret name | Value |
|---|---|
| `SHINYAPPS_TOKEN` | Your shinyapps.io token |
| `SHINYAPPS_SECRET` | Your shinyapps.io secret |

### 3. (Optional) Enable push-triggered deploys from `celiac-repository`

If you want data changes in `celiac-repository` to immediately trigger a webapp redeploy (rather than waiting for the next weekly run):

1. Create a GitHub Personal Access Token (PAT) with `repo` scope
2. In the `celiac-repository` repo, add it as a secret named `WEBAPP_DISPATCH_TOKEN`
3. Add the `.github/workflows/notify-webapp.yml` file to `celiac-repository`

Without this step, the webapp still refreshes on its weekly schedule (Monday 00:00 UTC) and can be triggered manually at any time from the Actions tab.

## How It Works

```
celiac-repository (data push)
        │
        ▼ (repository_dispatch)
celiac-webapp GitHub Action
        │
        ├─ fetch_repo_data.py (downloads latest TSVs)
        ├─ verify files exist
        └─ rsconnect::deployApp() → shinyapps.io
```

## Testing

To test without waiting for the schedule:
1. Go to the `celiac-webapp` repo → **Actions** tab
2. Select "Refresh Data and Deploy"
3. Click **Run workflow**

## Notes

- The `.gitignore` keeps data files out of git (they're fetched fresh each deploy)
- The `forceUpdate = TRUE` flag ensures deployment proceeds even if no app code changed (since data is fetched fresh, not committed, this avoids rsconnect skipping the deploy)
- shinyapps.io tokens do not expire on their own; they persist until manually revoked from the dashboard
- If you revoke and regenerate tokens, update the GitHub secrets to match
- GitHub sends email notifications to repo owners when scheduled workflows fail (enabled by default), so you'll know if a deploy breaks
- Deployment on shinyapps.io is atomic (new instance starts, then traffic swaps) so there is no user-visible downtime during a deploy
