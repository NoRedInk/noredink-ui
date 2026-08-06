# Cave Echo

Cave Echo is a conversational assistant that uses the [Assistants API](https://platform.openai.com/docs/assistants/overview) to generate responses about `noredink-ui`.

## Quick Start

If you have already set up the assistant, you can run it with the following commands:

```bash
source venv/bin/activate
python3 assistant.py
```

To exit the assistant, type `exit` and press Enter.

To deactivate the virtual environment, run `deactivate`.

## First-time Setup:

### Prerequisites

Before you begin, ensure you have the following:

- Python 3.7 or later. You can install the latest version of Python using Homebrew on macOS with `brew install python`. To check your Python version, run `python3 --version`.
- An OpenAI API key.

### Setup

#### OpenAI API Key

First, navigate to the `noredink-ui` root directory and create a `.env` file with your OpenAI API key:

```bash
cd path/to/noredink-ui
echo OPENAI_API_KEY=your_api_key_here > .env
```

There's a `.env.example` file in the root directory that you can use as a template:

```bash
cp .env.example .env
```

**Important:** Ensure the `.env` file is never committed to the repository by adding it to the `.gitignore` file if it isn't already.

#### Virtual Environment

It's recommended to use a virtual environment to manage dependencies without causing conflicts with other Python projects.

Navigate to the `assistant` directory if you haven't already:

```bash
cd path/to/noredink-ui/assistant
```

Create and activate a virtual environment:

```bash
python3 -m venv venv
source venv/bin/activate
```

**Important:** Ensure the `venv/` directory is never committed to the repository by adding it to the `.gitignore` file if it isn't already.

#### Dependencies

Install the required dependencies inside the virtual environment:

```bash
pip3 install openai python-dotenv
```

To deactivate the virtual environment, run `deactivate`.

### Running the Assistant

- Ensure you are in the `assistant` directory
- Activate the virtual environment if you haven't already: `source venv/bin/activate`
- Run `python3 assistant.py`

### Creating or Updating the Assistant

- Ensure you are in the `assistant` directory
- Activate the virtual environment if you haven't already: `source venv/bin/activate`
- To make changes to the assistant's behavior, edit the `create_or_update_assistant.py` file
- Run `python3 create_or_update_assistant.py`
