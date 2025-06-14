import os
import asyncio
import logging
from dotenv import load_dotenv
from openai import AsyncOpenAI


logging.basicConfig(
    level=logging.INFO, format="%(asctime)s - %(levelname)s - %(message)s"
)


class NoHTTPRequestFilter(logging.Filter):
    def filter(self, record):
        return "HTTP Request:" not in record.getMessage()


for handler in logging.root.handlers:
    handler.addFilter(NoHTTPRequestFilter())


load_dotenv()


OPENAI_API_KEY = os.getenv("OPENAI_API_KEY")
if not OPENAI_API_KEY:
    logging.error(
        "The `OPENAI_API_KEY` environment variable is not set. For instructions on how to set it, refer to the README."
    )
    exit(1)


client = AsyncOpenAI(api_key=OPENAI_API_KEY)


ASSISTANT_NAME = "Cave Echo"


async def retrieve_assistant_by_name(name):
    try:
        assistants = await client.beta.assistants.list()
        for assistant in assistants.data:
            if assistant.name == name:
                return assistant
        return None
    except Exception as e:
        logging.error(f"Failed to retrieve assistant by name: {e}")
        return None


async def create_thread():
    try:
        thread = await client.beta.threads.create()
        return thread
    except Exception as e:
        logging.error(f"Failed to create thread: {e}")
        return None


async def create_message(thread_id, content):
    try:
        message = await client.beta.threads.messages.create(
            thread_id=thread_id, role="user", content=content
        )
        return message
    except Exception as e:
        logging.error(f"Failed to create message: {e}")
        return None


async def create_a_run(assistant_id, thread_id):
    try:
        run = await client.beta.threads.runs.create(
            assistant_id=assistant_id, thread_id=thread_id
        )
        return run
    except Exception as e:
        logging.error(f"Failed to create a run: {e}")
        return None


async def bat_animation():
    """Displays a moving bat emoji in the console."""
    frames = [
        "🦇    ",
        "  🦇  ",
        "    🦇",
        "  🦇  ",
    ]
    while True:
        for frame in frames:
            print(f"\r{frame}", end="", flush=True)
            await asyncio.sleep(0.3)  # animation speed


async def get_responses(thread_id, run_id):
    animation_task = None
    try:
        animation_task = asyncio.create_task(bat_animation())

        while True:
            run = await client.beta.threads.runs.retrieve(
                thread_id=thread_id, run_id=run_id
            )
            if run.status == "completed":
                break
            await asyncio.sleep(1)

        if animation_task:
            animation_task.cancel()
            try:
                await animation_task
            except asyncio.CancelledError:
                pass  # expected exception on task cancellation

        print("\r", end="", flush=True)  # clear the animation from the console

        messages = await client.beta.threads.messages.list(thread_id=thread_id)
        if messages.data:
            # the first message in the list is the latest response
            message = messages.data[0]
            if message.role == "assistant" and message.content:
                print(f"{ASSISTANT_NAME}: {message.content[0].text.value}")

    except Exception as e:
        logging.error(f"Failed to get responses: {e}")
        if animation_task:
            animation_task.cancel()
            try:
                await animation_task
            except asyncio.CancelledError:
                pass  # again, ignore the expected cancellation error

    finally:
        # ensure the line is clear of animation after exception or completion
        print("\r", end="")


async def delete_thread(client, thread_id):
    try:
        response = await client.beta.threads.delete(thread_id)
        logging.info(f"Thread {thread_id} deleted successfully.")
        return response
    except Exception as e:
        logging.error(f"Failed to delete thread {thread_id}: {e}")
        return None


async def main():
    assistant = await retrieve_assistant_by_name(ASSISTANT_NAME)
    if assistant is None:
        logging.info(
            f"Assistant {ASSISTANT_NAME} not found. Aborting. For instructions on how to create an assistant, refer to the README."
        )
        return

    logging.info(
        f"Entering the cave! Beware of bats. Type 'exit' to see the sunlight again."
    )
    thread = await create_thread()
    if thread is None:
        logging.error("Failed to create conversation thread.")
        return

    try:
        while True:
            user_input = input("You: ")
            if user_input.lower() == "exit":
                logging.info("Emerging from the cave, back to the daylight. Goodbye!")
                break

            await create_message(thread.id, user_input)
            run = await create_a_run(assistant.id, thread.id)
            if run is None:
                logging.error("Failed to create a run.")
                return
            await get_responses(thread.id, run.id)
    finally:
        await delete_thread(
            client, thread.id
        )  # ensure the thread is deleted when exiting


if __name__ == "__main__":
    asyncio.run(main())
