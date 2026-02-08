[![License](https://img.shields.io/github/license/christianjianelli/yaaic?label=License&color=success)](https://github.com/christianjianelli/yaai_cloud_rest/blob/main/LICENSE)

# yaai_cloud_rest - ABAP AI tools Cloud REST API
This repository contains the REST API required to run the [ABAP AI tools Cockpit](https://github.com/christianjianelli/yaai_cloud_cockpit). The Cockpit frontend requires this API to function.

## Installation
You can install the ABAP AI tools Cloud REST API into your ABAP Cloud system using abapGit. The BTP ABAP environment comes with a preinstalled official SAP distribution of abapGit.

**Steps:**
1. Create a package named `YAAI_CLOUD_REST`;

2. Open the abapGit Repositories view and click the button with the plus sign (Link new abapGit Repository...):

<p style="margin-left: 50px">
   <img src="docs/images/install1.png" alt="Installation Step 2" width="800px">
</p>

3. Enter the URL `https://github.com/christianjianelli/yaai_cloud_rest.git`:

<p style="margin-left: 50px">
   <img src="docs/images/install2.png" alt="Installation Step 3" width="500px">
</p>

4. Specify the package:

<p style="margin-left: 50px">
   <img src="docs/images/install3.png" alt="Installation Step 4" width="500px">
</p>

5. Click the **Next** button:

<p style="margin-left: 50px">
   <img src="docs/images/install4.png" alt="Installation Step 5" width="500px">
</p>

6. Select all object and click the **Finish** button:

<p style="margin-left: 50px">
   <img src="docs/images/install5.png" alt="Installation Step 6" width="500px">
</p>

7. Activate the imported objects as needed.

<p style="margin-left: 50px">
   <img src="docs/images/install6.png" alt="Installation Step 7" width="500px">
</p>

You have now successfully installed the `ABAP AI tools Cloud REST API`.